# Perfil de los reclamos por medicamentos posteriores al cambio de
# clasificación de julio de 2023 (departamento, régimen, edad, gravedad y
# características del afectado). Requiere scripts/07_descargar_reclamos.R.
#
# Genera data/processed/perfil_medicamentos.csv.gz: reclamos por dimensión,
# año, motivo específico, tipo de patología y marca de alto costo (para poder
# separar alto costo después con R/clasificacion_medicamentos.R). La columna
# `dimension` dice qué variable se agregó y `valor`, su categoría.

source(here::here("R", "setup.R"))
source(here("R", "supersalud.R"))

es_medicamento <- \(d) {
  grepl("MEDICAMENT", paste(d$motivo_especifico, d$tipo_motivo, d$subtipo_motivo)) &
    !grepl("^DISPOSITIVOS", d$tipo_motivo)
}

dimensiones <- c("depto", "regimen", "edad", "riesgo", "sexo", "educacion", "etnia", "poblacion_especial")

agregaciones <- purrr::map(dimensiones, \(dim) list(
  filtro = es_medicamento,
  grupos = c("anio", "motivo_especifico", "patologia_tipo", "alto_costo", dim)
)) |>
  purrr::set_names(dimensiones)

reclamos <- supersalud_agregar(agregaciones, archivos = dplyr::filter(supersalud_reclamos, corte == "post"))

purrr::imap(reclamos, \(tabla, dim) {
  tabla |> dplyr::rename(valor = dplyr::all_of(dim)) |> dplyr::mutate(dimension = dim, .before = valor)
}) |>
  purrr::list_rbind() |>
  write_csv(here("data", "processed", "perfil_medicamentos.csv.gz"))
