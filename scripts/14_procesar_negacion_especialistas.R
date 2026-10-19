# Perfil de los reclamos por negación o demora de citas con especialista, a
# partir de los reclamos de la Supersalud posteriores al cambio de clasificación
# de julio de 2023 (antes casi no existía la categoría de negación). Requiere
# scripts/07_descargar_reclamos.R.
#
# Genera data/processed/negacion_especialistas.csv.gz con el número de reclamos
# por dimensión (departamento, régimen, riesgo, patología, sexo, edad, etnia,
# nivel educativo y población especial), tipo (negación o demora) y año. Cada
# dimensión se agrega por separado: la columna `dimension` dice cuál es y
# `valor`, su categoría.

source(here::here("R", "setup.R"))
source(here("R", "supersalud.R"))

es_cita_especialista <- \(d) {
  grepl("^ESPECIALIDADES M", d$tipo_motivo) &
    d$motivo_especifico %in% c("NEGACIÓN EN LA ASIGNACIÓN DE CITAS O CONSULTAS",
                               "FALTA DE OPORTUNIDAD EN LAS CITAS O CONSULTAS")
}

dimensiones <- c("depto", "regimen", "riesgo", "patologia_tipo", "sexo", "edad",
                 "etnia", "educacion", "poblacion_especial", "subtipo_motivo")

agregaciones <- purrr::map(dimensiones, \(dim) list(
  filtro = es_cita_especialista,
  grupos = c("anio", "motivo_especifico", dim)
)) |>
  purrr::set_names(dimensiones)

# Riesgo por especialidad: qué especialidades concentran negaciones de casos graves.
agregaciones$riesgo_especialidad <- list(
  filtro = es_cita_especialista,
  grupos = c("anio", "motivo_especifico", "riesgo", "subtipo_motivo")
)

reclamos <- supersalud_agregar(agregaciones, archivos = dplyr::filter(supersalud_reclamos, corte == "post"))

perfil <- purrr::imap(reclamos[dimensiones], \(tabla, dim) {
  tabla |> dplyr::rename(valor = dplyr::all_of(dim)) |> dplyr::mutate(dimension = dim, .before = valor)
}) |>
  purrr::list_rbind() |>
  dplyr::mutate(tipo = dplyr::if_else(grepl("^NEGACI", motivo_especifico), "Negación", "Demora"), .keep = "unused")

write_csv(perfil, here("data", "processed", "negacion_especialistas.csv.gz"))
write_csv(
  reclamos$riesgo_especialidad |> dplyr::mutate(tipo = dplyr::if_else(grepl("^NEGACI", motivo_especifico), "Negación", "Demora"), .keep = "unused"),
  here("data", "processed", "negacion_riesgo_especialidad.csv.gz")
)

perfil |> dplyr::filter(dimension == "regimen") |> dplyr::count(valor, tipo, wt = n) |> print(n = 30)
