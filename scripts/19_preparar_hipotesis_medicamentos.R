# Prepara los datos de las secciones "Perfil de los reclamos de alto costo" e
# "Hipótesis" de medicamentos.qmd. Requiere scripts/09, 11, 16, 18 y 20. Escribe
# en data/processed/ los archivos medh_*.csv.

source(here::here("R", "setup.R"))
source(here("R", "poblacion.R"))
source(here("R", "afiliados.R"))
source(here("R", "eps.R"))
source(here("R", "clasificacion_medicamentos.R"))

clasificar <- \(datos) {
  datos |>
    mutate(
      grupo = grupo_patologia(patologia_tipo),
      grupo = if_else(grupo %in% grupos_alto_costo, grupo, coalesce(grupo_intervencion(alto_costo), grupo)),
      es_alto_costo = grupo %in% c(grupos_alto_costo, "Otras intervenciones de alto costo"),
      motivo = categoria_motivo(motivo_especifico)
    )
}

# Perfil (julio de 2023 a diciembre de 2025) ---------------------------------------

perfil <- read_csv(here("data", "processed", "perfil_medicamentos.csv.gz"),
                   col_types = cols(.default = "c", n = "i")) |>
  clasificar() |>
  mutate(valor = str_squish(valor))

# Departamentos (2025): reclamos por 100.000 habitantes, todos y de alto costo.
poblacion <- dane_poblacion() |> filter(anio == 2025)
perfil |>
  filter(dimension == "depto", anio == "2025") |>
  rename(cod_depto = valor) |>
  summarise(todos = sum(n), alto_costo = sum(n[es_alto_costo]), .by = cod_depto) |>
  mutate(cod_depto = str_pad(cod_depto, 2, pad = "0")) |>
  inner_join(poblacion, by = "cod_depto") |>
  mutate(
    todos_100k = todos / poblacion * 1e5,
    alto_costo_100k = alto_costo / poblacion * 1e5,
    todos_nacional = sum(todos) / sum(poblacion) * 1e5,
    alto_costo_nacional = sum(alto_costo) / sum(poblacion) * 1e5
  ) |>
  select(cod_depto, departamento, poblacion, todos, alto_costo, todos_100k, alto_costo_100k, todos_nacional, alto_costo_nacional) |>
  write_csv(here("data", "processed", "medh_departamento.csv"))

# Régimen: reclamos de 2025 por 10.000 afiliados y proporción de negación de la entrega.
afiliados_regimen <- adres_afiliados() |> summarise(afiliados = sum(afiliados), .by = regimen)
perfil |>
  filter(dimension == "regimen", valor %in% c("Contributivo", "Subsidiado", "Régimen Especial", "Régimen especial")) |>
  mutate(regimen = str_to_sentence(valor)) |>
  summarise(
    reclamos = sum(n), alto_costo = sum(n[es_alto_costo]),
    pct_negacion_entrega = sum(n[motivo == "Negación de la entrega"]) / sum(n),
    alto_costo_2025 = sum(n[es_alto_costo & anio == "2025"]),
    .by = regimen
  ) |>
  left_join(afiliados_regimen, by = "regimen") |>
  mutate(alto_costo_10k_2025 = alto_costo_2025 / afiliados * 1e4) |>
  write_csv(here("data", "processed", "medh_regimen.csv"), na = "")

# Gravedad.
perfil |>
  filter(dimension == "riesgo") |>
  mutate(riesgo = str_to_sentence(valor)) |>
  summarise(reclamos = sum(n), alto_costo = sum(n[es_alto_costo]),
            pct_negacion_entrega = sum(n[motivo == "Negación de la entrega"]) / sum(n),
            .by = riesgo) |>
  write_csv(here("data", "processed", "medh_riesgo.csv"))

# Población y características del afectado: proporción de negación de la
# entrega en los reclamos de alto costo (sin "No aplica" ni categorías pequeñas).
etiquetas <- c(edad = "Edad", educacion = "Nivel educativo", poblacion_especial = "Población especial",
               etnia = "Grupo étnico", sexo = "Sexo")
perfil |>
  filter(dimension %in% names(etiquetas), es_alto_costo) |>
  mutate(valor = str_to_sentence(valor)) |>
  summarise(reclamos = sum(n), negacion_entrega = sum(n[motivo == "Negación de la entrega"]),
            .by = c(dimension, valor)) |>
  filter(!valor %in% c("No aplica", "No reporta"), reclamos >= 200) |>
  mutate(dimension = etiquetas[dimension], pct_negacion_entrega = negacion_entrega / reclamos) |>
  write_csv(here("data", "processed", "medh_poblacion.csv"))

# Hipótesis ---------------------------------------------------------------------------

reclamos <- read_csv(here("data", "processed", "reclamos_medicamentos.csv.gz"),
                     col_types = cols(.default = "c", n = "i")) |>
  filter(corte == "post", !str_detect(coalesce(tipo_motivo, ""), "^DISPOSITIVOS")) |>
  clasificar() |>
  mutate(fecha = suppressWarnings(make_date(as.integer(anio), as.integer(mes), 1))) |>
  filter(!is.na(fecha))

# H. barreras administrativas: participación de cada motivo por semestre.
reclamos |>
  mutate(semestre = paste0(year(fecha), "-", if_else(month(fecha) <= 6, "I", "II"))) |>
  count(semestre, motivo, wt = n) |>
  mutate(participacion = n / sum(n), .by = semestre) |>
  write_csv(here("data", "processed", "medh_motivo_semestre.csv"))

# H. cadena de dispensación: ruptura entre Famisanar y Colsubsidio (1 de agosto
# de 2025). Reclamos mensuales como índice respecto a enero-julio de 2025,
# Famisanar frente a EPS no intervenidas.
control <- c("Aliansalud", "Cajacopi", "Capital Salud", "Comfenalco Valle", "Compensar", "Mutual Ser", "Salud Total", "Sura")
reclamos |>
  mutate(eps = eps_canonico(eps),
         serie = case_when(eps == "Famisanar" ~ "Famisanar", eps %in% control ~ "EPS no intervenidas (control)")) |>
  filter(!is.na(serie), fecha >= as.Date("2024-07-01")) |>
  count(serie, fecha, wt = n) |>
  mutate(indice = n / mean(n[fecha >= as.Date("2025-01-01") & fecha < as.Date("2025-08-01")]), .by = serie) |>
  write_csv(here("data", "processed", "medh_evento_famisanar.csv"))

# H. recursos y deudas: reclamos por medicamentos por 10.000 afiliados (2025)
# frente a finanzas de diciembre de 2025.
finanzas <- read_csv(here("data", "processed", "eps_finanzas.csv"), show_col_types = FALSE) |>
  filter(corte == as.Date("2025-12-01"), ingresos > 0)
read_csv(here("data", "processed", "med_alto_costo_eps.csv"), show_col_types = FALSE) |>
  mutate(eps = eps_canonico(eps)) |>
  inner_join(finanzas, by = "eps") |>
  filter(afiliados >= 2e5) |>
  transmute(eps, afiliados, alto_costo_10k, medicamentos_10k, siniestralidad,
            patrimonio_por_afiliado = patrimonio / afiliados, meses_deuda, formato) |>
  write_csv(here("data", "processed", "medh_eps_finanzas.csv"))

# Medicamentos no financiados con la UPC (antes "no POS"): reclamos por año.
# La categoría se reduce porque entre 2021 y 2023 muchos medicamentos pasaron a
# ser financiados con la UPC; no es un indicador estable de medicamentos caros.
read_csv(here("data", "processed", "reclamos_medicamentos.csv.gz"),
         col_types = cols(.default = "c", n = "i")) |>
  filter(!str_detect(coalesce(tipo_motivo, ""), "^DISPOSITIVOS")) |>
  mutate(
    anio = as.integer(as.numeric(sub(",", ".", anio))),
    no_upc = if_else(corte == "post", coalesce(tipo_motivo == "MEDICAMENTOS NO UPC", FALSE),
                     str_detect(coalesce(motivo_especifico, ""), "NO.?POS"))
  ) |>
  filter(!is.na(anio)) |>
  summarise(no_upc = sum(n[no_upc]), total = sum(n), .by = anio) |>
  mutate(participacion = no_upc / total) |>
  arrange(anio) |>
  write_csv(here("data", "processed", "medh_no_upc_anual.csv"))

# ¿Es real el aumento de 2025? -------------------------------------------------------

# Reclamos por medicamentos frente al total de reclamos ante la Supersalud y a la
# población: si el canal de quejas se usa más, todo crece a la vez.
anio_num <- \(x) as.integer(as.numeric(sub(",", ".", x)))
medicamentos_anual <- read_csv(here("data", "processed", "reclamos_medicamentos.csv.gz"),
                               col_types = cols(.default = "c", n = "i")) |>
  filter(!str_detect(coalesce(tipo_motivo, ""), "^DISPOSITIVOS")) |>
  clasificar() |>
  mutate(anio = anio_num(anio))
read_csv(here("data", "processed", "reclamos_totales_medicamentos.csv.gz"), col_types = cols(.default = "c", n = "i")) |>
  mutate(anio = anio_num(anio)) |>
  summarise(total = sum(n), .by = anio) |>
  inner_join(summarise(medicamentos_anual, medicamentos = sum(n), alto_costo = sum(n[es_alto_costo]), .by = anio), by = "anio") |>
  inner_join(summarise(dane_poblacion(), poblacion = sum(poblacion), .by = anio), by = "anio") |>
  mutate(pct_medicamentos = medicamentos / total, pct_alto_costo = alto_costo / total,
         alto_costo_100k = alto_costo / poblacion * 1e5) |>
  arrange(anio) |>
  write_csv(here("data", "processed", "medh_anual_normalizado.csv"))

# Aporte de cada EPS al aumento de los reclamos de alto costo entre 2024 y 2025.
afiliados_eps <- adres_afiliados() |>
  mutate(eps = eps_canonico(ent_nombre)) |>
  summarise(afiliados = sum(afiliados), .by = eps)
medicamentos_anual |>
  filter(es_alto_costo, anio %in% 2024:2025) |>
  mutate(eps = coalesce(eps_canonico(eps), "Otras")) |>
  count(eps, anio, wt = n) |>
  pivot_wider(names_from = anio, values_from = n, names_prefix = "reclamos_", values_fill = 0) |>
  mutate(aumento = reclamos_2025 - reclamos_2024, pct_del_aumento = aumento / sum(aumento)) |>
  left_join(afiliados_eps, by = "eps") |>
  mutate(pct_afiliados = afiliados / sum(afiliados_eps$afiliados[!is.na(afiliados_eps$eps)])) |>
  arrange(desc(aumento)) |>
  write_csv(here("data", "processed", "medh_aumento_eps.csv"), na = "")

# Panel de deuda y reclamos -----------------------------------------------------------

# Como en las citas (scripts/17): deuda de cada EPS al cierre de un semestre
# frente a sus reclamos de alto costo del semestre siguiente, con efectos fijos
# de EPS y de semestre. Solo EPS en formato NIIF y semestres con al menos 30
# reclamos. Errores estándar agrupados por EPS.
reclamos_semestre <- reclamos |>
  mutate(eps = eps_canonico(eps), periodo = paste0(year(fecha), "-", if_else(month(fecha) <= 6, 1, 2))) |>
  filter(!is.na(eps)) |>
  summarise(alto_costo = sum(n[es_alto_costo]), medicamentos = sum(n), .by = c(eps, periodo))
panel <- read_csv(here("data", "processed", "eps_finanzas.csv"), show_col_types = FALSE) |>
  filter(ingresos > 0, formato == "niif", !is.na(eps)) |>
  mutate(periodo = if_else(month(corte) == 6, paste0(year(corte), "-2"), paste0(year(corte) + 1, "-1"))) |>
  select(eps, periodo, meses_deuda) |>
  inner_join(reclamos_semestre, by = c("eps", "periodo"))

ee_agrupado <- \(modelo, grupo) {
  X <- model.matrix(modelo)
  e <- resid(modelo)
  pan <- solve(crossprod(X))
  carne <- Reduce(`+`, map(split(seq_along(e), grupo), \(i) tcrossprod(crossprod(X[i, , drop = FALSE], e[i]))))
  g <- n_distinct(grupo)
  sqrt(diag(pan %*% carne %*% pan * g / (g - 1) * (nrow(X) - 1) / (nrow(X) - ncol(X))))
}
map(c("alto_costo", "medicamentos"), \(y) {
  datos <- filter(panel, .data[[y]] >= 30)
  modelo <- lm(reformulate(c("meses_deuda", "factor(eps)", "factor(periodo)"), paste0("log(", y, ")")), datos)
  b <- unname(coef(modelo)["meses_deuda"])
  ee <- unname(ee_agrupado(modelo, datos$eps)["meses_deuda"])
  tibble(reclamos = y, coef = b, ee, cambio_pct = exp(b) - 1, ic_bajo = exp(b - 1.96 * ee) - 1,
         ic_alto = exp(b + 1.96 * ee) - 1, observaciones = nrow(datos), eps = n_distinct(datos$eps))
}) |>
  list_rbind() |>
  write_csv(here("data", "processed", "medh_panel_resumen.csv"))

# Reclamos por paciente (Cuenta de Alto Costo) ------------------------------------------

# Reclamos por medicamentos de 2025 por cada 10.000 pacientes de cada
# enfermedad. Los cortes de la CAC van de julio de 2024 a enero de 2026 según
# la enfermedad (ver data/referencia/cac_pacientes.csv). La mitad de los
# reclamos no especifica patología: las tasas son un piso.
cac <- read_csv(here("data", "referencia", "cac_pacientes.csv"), show_col_types = FALSE)
reclamos_2025 <- filter(reclamos, year(fecha) == 2025) |>
  mutate(patologia = toupper(str_squish(patologia_tipo)))
map(seq_len(nrow(cac)), \(i) {
  r <- filter(reclamos_2025, str_detect(coalesce(patologia, ""), cac$patron[i]))
  tibble(enfermedad = cac$enfermedad[i], reclamos = sum(r$n),
         negacion_entrega = sum(r$n[r$motivo == "Negación de la entrega"]),
         pacientes = cac$pacientes[i], corte = cac$corte[i])
}) |>
  list_rbind() |>
  mutate(por_10k = reclamos / pacientes * 1e4, pct_negacion_entrega = negacion_entrega / reclamos) |>
  arrange(desc(por_10k)) |>
  write_csv(here("data", "processed", "medh_cac_pacientes.csv"))

# Desabastecimiento y reclamos por enfermedad -------------------------------------------

# ¿Suben los reclamos de una enfermedad el mes siguiente a que uno de sus
# medicamentos de referencia aparezca desabastecido o en riesgo en el listado
# del Invima? Panel enfermedad × mes con efectos fijos de enfermedad y de mes.
patron_enfermedad <- c(
  "Artritis reumatoide" = "ARTRITIS REUMATOIDEA",
  "Enfermedad renal y trasplante" = "^IRC|RENAL CR[OÓ]NICA|TRASPLANTE",
  "Cáncer de mama" = "C[AÁ]NCER DE MAMA",
  "VIH" = "VIH",
  "Cáncer de próstata" = "PR[OÓ]STATA",
  "Leucemias" = "LEUCEMIA",
  "Cáncer de pulmón" = "C[AÁ]NCER.*PULM",
  "Cáncer colorrectal" = "COLORECTAL|COLORRECTAL"
)
desab <- read_csv(here("data", "processed", "med_desab_referencia_mensual.csv"), show_col_types = FALSE)
write_csv(
  desab |>
    filter(any(estado != "Sin reporte"), .by = c(enfermedad, medicamento)) |>
    mutate(problemas = sum(estado %in% c("Desabastecido", "En riesgo de desabastecimiento")), .by = c(enfermedad, medicamento)),
  here("data", "processed", "medh_desab_referencia.csv")
)
reclamos_enfermedad <- imap(patron_enfermedad, \(patron, e) {
  reclamos |>
    filter(str_detect(toupper(str_squish(coalesce(patologia_tipo, ""))), patron)) |>
    count(fecha, wt = n) |>
    mutate(enfermedad = e)
}) |>
  list_rbind()
panel_desab <- desab |>
  summarise(problema = any(estado %in% c("Desabastecido", "En riesgo de desabastecimiento")), .by = c(enfermedad, fecha)) |>
  mutate(fecha = fecha %m+% months(1)) |>
  inner_join(reclamos_enfermedad, by = c("enfermedad", "fecha"))
modelo <- lm(log(n) ~ problema + factor(enfermedad) + factor(fecha), panel_desab)
b <- unname(coef(modelo)["problemaTRUE"])
ee <- unname(ee_agrupado(modelo, panel_desab$enfermedad)["problemaTRUE"])
tibble(coef = b, ee, cambio_pct = exp(b) - 1, ic_bajo = exp(b - 1.96 * ee) - 1, ic_alto = exp(b + 1.96 * ee) - 1,
       observaciones = nrow(panel_desab), meses_con_problema = sum(panel_desab$problema),
       enfermedades = n_distinct(panel_desab$enfermedad)) |>
  write_csv(here("data", "processed", "medh_desab_panel.csv"))
