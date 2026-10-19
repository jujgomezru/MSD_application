# Prepara los datos de la sección "Perfil de la negación" de index.qmd a partir
# de scripts/14_procesar_negacion_especialistas.R. Escribe en data/processed/
# los archivos neg_*.csv.
#
# Para comparar grupos sin conocer su población se usa la proporción de
# negación: negaciones sobre el total de reclamos por negación o demora de citas
# con especialista. Donde sí hay denominador (departamentos, regímenes) se
# calculan tasas.

source(here::here("R", "setup.R"))
source(here("R", "poblacion.R"))
source(here("R", "afiliados.R"))
source(here("R", "clasificacion_medicamentos.R"))

perfil <- read_csv(here("data", "processed", "negacion_especialistas.csv.gz"),
                   col_types = cols(.default = "c", n = "i")) |>
  mutate(valor = str_squish(valor))

proporcion <- \(datos) {
  datos |>
    summarise(negaciones = sum(n[tipo == "Negación"]), total = sum(n), .by = c(dimension, valor)) |>
    mutate(pct_negacion = negaciones / total)
}

# Departamentos (2025): negaciones por 100.000 habitantes.
poblacion <- dane_poblacion() |> filter(anio == 2025)
neg_depto <- perfil |>
  filter(dimension == "depto", anio == "2025", tipo == "Negación") |>
  count(cod_depto = str_pad(valor, 2, pad = "0"), wt = n, name = "negaciones") |>
  inner_join(poblacion, by = "cod_depto") |>
  mutate(por_100k = negaciones / poblacion * 1e5)
neg_depto |>
  mutate(tasa_nacional = sum(negaciones) / sum(poblacion) * 1e5) |>
  select(cod_depto, departamento, negaciones, poblacion, por_100k, tasa_nacional) |>
  write_csv(here("data", "processed", "neg_departamento.csv"))

# Régimen: proporción de negación (todo el periodo) y negaciones de 2025 por
# 10.000 afiliados (BDUA; el régimen especial no está en la BDUA).
afiliados_regimen <- adres_afiliados() |> summarise(afiliados = sum(afiliados), .by = regimen)
perfil |>
  filter(dimension == "regimen", valor %in% c("Contributivo", "Subsidiado", "Régimen Especial")) |>
  proporcion() |>
  left_join(
    perfil |> filter(dimension == "regimen", anio == "2025", tipo == "Negación") |>
      count(valor, wt = n, name = "negaciones_2025"),
    by = "valor"
  ) |>
  left_join(afiliados_regimen, by = c("valor" = "regimen")) |>
  mutate(por_10k_2025 = negaciones_2025 / afiliados * 1e4) |>
  select(regimen = valor, negaciones, total, pct_negacion, negaciones_2025, afiliados, por_10k_2025) |>
  write_csv(here("data", "processed", "neg_regimen.csv"), na = "")

# Gravedad: proporción por clasificación de riesgo y especialidades con más
# negaciones de casos priorizados o de riesgo vital.
perfil |>
  filter(dimension == "riesgo") |>
  proporcion() |>
  transmute(riesgo = str_to_sentence(valor), negaciones, total, pct_negacion) |>
  write_csv(here("data", "processed", "neg_riesgo.csv"))

read_csv(here("data", "processed", "negacion_riesgo_especialidad.csv.gz"),
         col_types = cols(.default = "c", n = "i")) |>
  filter(tipo == "Negación", riesgo %in% c("PRIORIZADO", "RIESGO VITAL"), !is.na(subtipo_motivo)) |>
  count(especialidad = str_to_sentence(str_squish(subtipo_motivo)), wt = n, name = "negaciones") |>
  slice_max(negaciones, n = 15) |>
  write_csv(here("data", "processed", "neg_priorizados_especialidad.csv"))

# Enfermedad del afectado, agrupada como en medicamentos, pero separando las
# cardiovasculares de las metabólicas (diabetes, obesidad y sobrepeso). La
# medición de factores de riesgo no es una enfermedad y queda aparte.
perfil |>
  filter(dimension == "patologia_tipo") |>
  mutate(
    grupo = grupo_patologia(valor),
    valor = case_when(
      grupo != "Cardiovasculares y metabólicas" ~ grupo,
      str_detect(toupper(valor), "FACTORES DE RIESGO") ~ "Medición de factores de riesgo",
      str_detect(toupper(valor), "^DIABETES|OBESIDAD|SOBREPESO") ~ "Metabólicas",
      .default = "Cardiovasculares"
    )
  ) |>
  proporcion() |>
  filter(valor != "Sin patología especificada") |>
  transmute(grupo = valor, alto_costo = grupo %in% grupos_alto_costo, negaciones, total, pct_negacion) |>
  arrange(desc(negaciones)) |>
  write_csv(here("data", "processed", "neg_enfermedad.csv"))

# Población vulnerable y características del afectado. Se excluye "No aplica"
# y las categorías con menos de 300 reclamos.
etiquetas <- c(edad = "Edad", educacion = "Nivel educativo", poblacion_especial = "Población especial",
               etnia = "Grupo étnico", sexo = "Sexo")
perfil |>
  filter(dimension %in% names(etiquetas)) |>
  mutate(valor = str_to_sentence(valor)) |>
  proporcion() |>
  filter(!valor %in% c("No aplica", "No reporta", "Sin información"), total >= 300) |>
  mutate(dimension = etiquetas[dimension]) |>
  cross_join(
    perfil |> summarise(pct_general = sum(n[tipo == "Negación" & dimension == "regimen"]) / sum(n[dimension == "regimen"]))
  ) |>
  arrange(dimension, desc(pct_negacion)) |>
  write_csv(here("data", "processed", "neg_poblacion.csv"))

# Pacientes crónicos (Cuenta de Alto Costo): reclamos de julio de 2024 a junio
# de 2025 por cada 10.000 pacientes de cada enfermedad. Los cortes de la CAC van
# de julio de 2024 a enero de 2026 según la enfermedad (ver
# data/referencia/cac_pacientes.csv). Como referencia, los reclamos con alguna patología
# especificada por cada 10.000 afiliados (BDUA). Cerca del 70 % de los reclamos
# no especifica patología, así que las tasas son un piso; sirven para comparar
# enfermedades entre sí.
cac <- read_csv(here("data", "referencia", "cac_pacientes.csv"), show_col_types = FALSE)
patologia_cac <- perfil |>
  filter(dimension == "patologia_tipo", periodo %in% c("2024-2", "2025-1"))
sin_patologia <- c("OTROS", "PROBLEMAS RELACIONADOS CON FACILIDADES DE ATENCIÓN MÉDICA U OTROS SERVICIOS DE SALUD")
referencia_cac <- patologia_cac |>
  filter(!valor %in% sin_patologia, !is.na(valor)) |>
  summarise(enfermedad = "Afiliados en general",
            negaciones = sum(n[tipo == "Negación"]), total = sum(n), pacientes = sum(adres_afiliados()$afiliados))
map(seq_len(nrow(cac)), \(i) {
  r <- filter(patologia_cac, str_detect(toupper(valor), cac$patron[i]))
  tibble(enfermedad = cac$enfermedad[i], negaciones = sum(r$n[r$tipo == "Negación"]), total = sum(r$n),
         pacientes = cac$pacientes[i])
}) |>
  list_rbind() |>
  bind_rows(referencia_cac) |>
  mutate(por_10k = total / pacientes * 1e4, negaciones_10k = negaciones / pacientes * 1e4,
         pct_negacion = negaciones / total, referencia = str_starts(enfermedad, "Afiliados")) |>
  arrange(referencia, desc(por_10k)) |>
  write_csv(here("data", "processed", "neg_cronicos_cac.csv"))
