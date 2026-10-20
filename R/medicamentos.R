descargar_datos_gov_csv <- function(id, destino, limite = 1e6) {
  dir.create(dirname(destino), recursive = TRUE, showWarnings = FALSE)
  httr2::request(sprintf("https://www.datos.gov.co/resource/%s.csv", id)) |>
    httr2::req_url_query(`$limit` = format(limite, scientific = FALSE)) |>
    httr2::req_timeout(600) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_perform(path = destino) |>
    invisible()
}

invima_desabastecimiento_pagina <- "https://www.invima.gov.co/productos-vigilados/medicamentos-y-productos-biologicos/desabastecimientos"
invima_desabastecimiento_extra <- "https://www.minsalud.gov.co/sites/rid/Lists/BibliotecaDigital/RIDE/VS/MET/listado-abastecimiento-desabastecimiento-medicamentos-2026.pdf"

invima_descargar_desabastecimiento <- function(carpeta = here::here("data", "raw", "invima", "desabastecimiento")) {
  dir.create(carpeta, recursive = TRUE, showWarnings = FALSE)
  pagina <- httr2::request(invima_desabastecimiento_pagina) |>
    httr2::req_user_agent("Mozilla/5.0") |>
    httr2::req_perform() |>
    httr2::resp_body_string()
  enlaces <- unique(regmatches(pagina, gregexpr('/invima_website/static/attachments/medicamentos_desabastecimientos/[^"]+\\.pdf', pagina))[[1]])
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

invima_leer_listado <- function(archivo) {
  paginas <- pdftools::pdf_text(archivo)
  lineas <- unlist(strsplit(paginas, "\n"))
  estado_re <- "(?i)\\b(no desabastecido|desabastecido|en riesgo de|en monitorizaci.n)"
  tiene_fecha <- grepl("\\d{1,2}/\\d{1,2}/\\d{2,4}", lineas)
  ancla <- which(grepl(estado_re, lineas, perl = TRUE) &
                   (tiene_fecha | dplyr::lag(tiene_fecha, default = FALSE) | dplyr::lead(tiene_fecha, default = FALSE)))
  previa <- c(0, utils::head(ancla, -1))

  titulo <- tolower(stringi::stri_trans_general(paste(paginas[1:2], collapse = " "), "Latin-ASCII"))
  mes <- stringr::str_match(titulo, paste0("desabastecimiento\\s+(?:de\\s+medicamentos\\s+)?(?:de\\s+)?",
                                           "(enero|febrero|marzo|abril|mayo|junio|julio|agosto|septiembre|octubre|noviembre|diciembre)",
                                           "\\s+(?:de\\s+)?(20\\d\\d)"))
  meses <- c("enero", "febrero", "marzo", "abril", "mayo", "junio", "julio", "agosto",
             "septiembre", "octubre", "noviembre", "diciembre")

  tibble::tibble(
    fecha = lubridate::make_date(as.integer(mes[3]), match(mes[2], meses), 1),
    estado = dplyr::case_match(
      tolower(stringr::str_extract(lineas[ancla], estado_re)),
      "no desabastecido" ~ "No desabastecido",
      "desabastecido" ~ "Desabastecido",
      "en riesgo de" ~ "En riesgo de desabastecimiento",
      .default = "En monitorización"
    ),
    nombre = purrr::map2_chr(previa, ancla, \(a, b) stringr::str_squish(paste(substr(lineas[(a + 1):b], 1, 95), collapse = " ")))
  )
}
