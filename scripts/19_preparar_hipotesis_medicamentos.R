# Prepara los datos de las secciones "Perfil de los reclamos de alto costo" e
# "Hipótesis" de medicamentos.qmd. Requiere scripts/09, 11, 16 y 18. Escribe en
# data/processed/ los archivos medh_*.csv.

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
