# Fuentes sobre disponibilidad y precio de medicamentos (medicamentos.qmd).

# Descarga un conjunto de datos completo de datos.gov.co como CSV.
descargar_datos_gov_csv <- function(id, destino, limite = 1e6) {
  dir.create(dirname(destino), recursive = TRUE, showWarnings = FALSE)
  httr2::request(sprintf("https://www.datos.gov.co/resource/%s.csv", id)) |>
    httr2::req_url_query(`$limit` = format(limite, scientific = FALSE)) |>
    httr2::req_timeout(600) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_perform(path = destino) |>
    invisible()
}

# Listados mensuales de abastecimiento y desabastecimiento del Invima (PDF).
# Se toman los enlaces publicados en la página del Invima, más el listado de
# 2026 que publica el Minsalud. Descarga solo los que falten.
invima_desabastecimiento_pagina <- "https://www.invima.gov.co/productos-vigilados/medicamentos-y-productos-biologicos/desabastecimientos"
invima_desabastecimiento_extra <- "https://www.minsalud.gov.co/sites/rid/Lists/BibliotecaDigital/RIDE/VS/MET/listado-abastecimiento-desabastecimiento-medicamentos-2026.pdf"

invima_descargar_desabastecimiento <- function(carpeta = here::here("data", "raw", "invima", "desabastecimiento")) {
  dir.create(carpeta, recursive = TRUE, showWarnings = FALSE)
  pagina <- httr2::request(invima_desabastecimiento_pagina) |>
    httr2::req_user_agent("Mozilla/5.0") |>
    httr2::req_perform() |>
    httr2::resp_body_string()
  enlaces <- unique(regmatches(pagina, gregexpr('/invima_website/static/attachments/medicamentos_desabastecimientos/[^"]+\\.pdf', pagina))[[1]])
  # Solo los listados mensuales (no los informes temáticos ni los consolidados de 2022).
  enlaces <- enlaces[grepl("listado", enlaces, ignore.case = TRUE)]
  urls <- c(paste0("https://www.invima.gov.co", enlaces), invima_desabastecimiento_extra)

  for (url in urls) {
    destino <- file.path(carpeta, basename(url))
    if (file.exists(destino)) next
    httr2::request(url) |>
      httr2::req_user_agent("Mozilla/5.0") |>
      httr2::req_timeout(300) |>
      httr2::req_retry(max_tries = 3) |>
      httr2::req_perform(path = destino)
  }
  invisible(urls)
}
