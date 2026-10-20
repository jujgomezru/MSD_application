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
