dane_poblacion_url <- "https://www.dane.gov.co/files/censo2018/proyecciones-de-poblacion/Departamental/PPED-AreaDep-2018-2050_VP.xlsx"

dane_poblacion <- function(archivo = here::here("data", "raw", "dane", "PPED-AreaDep-2018-2050_VP.xlsx")) {
  if (!file.exists(archivo)) {
    dir.create(dirname(archivo), recursive = TRUE, showWarnings = FALSE)
    utils::download.file(dane_poblacion_url, archivo, mode = "wb")
  }

  hoja <- readxl::read_excel(archivo, sheet = "PobDepartamentalxÁrea",
                             col_names = FALSE, col_types = "text", .name_repair = "minimal")
  fila <- which(hoja[[1]] == "DP")[1]
  tabla <- hoja[-seq_len(fila), ]
  names(tabla) <- unlist(hoja[fila, ])

  tabla |>
    janitor::clean_names() |>
    dplyr::filter(area_geografica == "Total", !is.na(dp)) |>
    dplyr::transmute(
      cod_depto = dp,
      departamento = dpnom,
      anio = as.integer(ano),
      poblacion = as.numeric(total)
    )
}
