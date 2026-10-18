# Descarga de los reportes de oportunidad de citas del Observatorio Nacional de
# Calidad en Salud (https://rssvr2.sispro.gov.co/IndicadoresMOCA/).
#
# La página es un ReportViewer de ASP.NET: cada filtro (indicador, departamento,
# municipio) es un postback que devuelve la página con el siguiente filtro
# habilitado. Estas funciones repiten esos postbacks y luego piden la exportación
# a CSV, igual que el botón "Exportar" del navegador.

library(httr2)
library(rvest)

moca_url <- "https://rssvr2.sispro.gov.co/IndicadoresMOCA/"
moca_reporte_ips <- "/Resolucion256-2016/IndicadoresIPS-ExperienciaAtencion-Resol256"
moca_prefijo <- "ctl00$MainContent$rptContenedor$"

# Indicadores de "asignación de cita" disponibles en el reporte por IPS.
# El nombre es el que se usa en los archivos; el valor, el texto del indicador.
moca_indicadores <- c(
  medicina_general = "Medicina General",
  medicina_interna = "Medicina Interna",
  pediatria        = "Pediatría",
  ginecologia      = "Ginecología",
  obstetricia      = "Obstetricia",
  cirugia_general  = "Cirugía General"
)

# Sesión y formulario ---------------------------------------------------------

moca_peticion <- function(cookies) {
  request(moca_url) |>
    req_user_agent("MSD_application (https://github.com/jujgomezru/MSD_application)") |>
    req_cookie_preserve(cookies) |>
    req_timeout(300) |>
    req_retry(max_tries = 3)
}

# Reúne los campos que el navegador enviaría en el siguiente postback.
moca_campos <- function(pagina) {
  inputs <- html_elements(pagina, "input")
  tipo <- html_attr(inputs, "type")
  nombre <- html_attr(inputs, "name")
  valor <- html_attr(inputs, "value")
  usar <- !is.na(nombre) & !(tipo %in% c("submit", "image", "checkbox"))
  campos <- as.list(setNames(dplyr::coalesce(valor[usar], ""), nombre[usar]))

  for (sel in html_elements(pagina, "select")) {
    elegido <- html_attr(html_element(sel, "option[selected]"), "value")
    campos[[html_attr(sel, "name")]] <- dplyr::coalesce(elegido, "")
  }

  # Campos que llena el JavaScript del visor; sin ellos el servidor responde 500.
  visibilidad <- paste0(moca_prefijo, "ctl09$VisibilityState$ctl00")
  if (is.null(campos[[visibilidad]]) || !nzchar(campos[[visibilidad]])) {
    campos[[visibilidad]] <- "None"
  }
  campos[[paste0(moca_prefijo, "ctl10")]] <- "ltr"
  campos[[paste0(moca_prefijo, "ctl11")]] <- "standards"
  campos[["__EVENTARGUMENT"]] <- ""
  campos
}

moca_postback <- function(cookies, campos, objetivo = "") {
  campos[["__EVENTTARGET"]] <- objetivo
  moca_peticion(cookies) |>
    req_body_form(!!!campos) |>
    req_perform() |>
    resp_body_html()
}

# Opciones de un <select>: data.frame con valor y etiqueta.
moca_opciones <- function(pagina, nombre) {
  opciones <- html_elements(pagina, sprintf("select[name='%s'] option", nombre))
  data.frame(
    valor = html_attr(opciones, "value"),
    etiqueta = stringr::str_squish(gsub(" ", " ", html_text(opciones)))
  )
}

# Descarga ----------------------------------------------------------------------

# Departamentos disponibles para un indicador (código DANE y etiqueta).
moca_departamentos <- function(indicador = "Pediatría") {
  cookies <- tempfile()
  on.exit(unlink(cookies))
  pagina <- moca_seleccionar_indicador(cookies, indicador)
  moca_opciones(pagina, paste0(moca_prefijo, "ctl04$ctl05$ddValue")) |>
    dplyr::filter(valor != "0") |>
    dplyr::mutate(codigo = dplyr::if_else(
      etiqueta == "Total país", "00", stringr::str_extract(etiqueta, "^\\d{2}")
    ))
}

moca_seleccionar_indicador <- function(cookies, indicador) {
  pagina <- moca_peticion(cookies) |> req_perform() |> resp_body_html()

  campos <- moca_campos(pagina)
  campos[["ctl00$MainContent$cmbReportes2"]] <- moca_reporte_ips
  pagina <- moca_postback(cookies, campos, "ctl00$MainContent$cmbReportes2")

  control <- paste0(moca_prefijo, "ctl04$ctl03$ddValue")
  opciones <- moca_opciones(pagina, control)
  valor <- opciones$valor[stringr::str_ends(opciones$etiqueta, paste("cita de", indicador))]
  if (length(valor) != 1) stop("Indicador no encontrado en el reporte: ", indicador)

  campos <- moca_campos(pagina)
  campos[[control]] <- valor
  moca_postback(cookies, campos, control)
}

# Descarga el CSV de un indicador y un departamento.
# `departamento` es el código DANE de 2 dígitos, o "00" para el total país.
moca_descargar <- function(indicador, departamento, destino, solo_total = TRUE) {
  cookies <- tempfile()
  on.exit(unlink(cookies))
  pagina <- moca_seleccionar_indicador(cookies, indicador)

  control <- paste0(moca_prefijo, "ctl04$ctl05$ddValue")
  opciones <- moca_opciones(pagina, control)
  valor <- if (departamento == "00") {
    opciones$valor[opciones$etiqueta == "Total país"]
  } else {
    opciones$valor[stringr::str_starts(opciones$etiqueta, paste0(departamento, " - "))]
  }
  if (length(valor) != 1) stop("Departamento no encontrado: ", departamento)

  campos <- moca_campos(pagina)
  campos[[control]] <- valor
  pagina <- moca_postback(cookies, campos, control)

  # Municipio es de selección múltiple. La casilla ctl00 es "(Select All)" y
  # solo aparece cuando hay más de una opción; la última opción es "Total".
  # Con solo_total = TRUE se pide únicamente el total del departamento (o del
  # país); si no, todos los municipios con su detalle por IPS.
  lista <- paste0(moca_prefijo, "ctl04$ctl07$divDropDown$")
  casillas <- html_attr(html_elements(pagina, "input[type='checkbox']"), "name")
  casillas <- casillas[startsWith(casillas, lista) & casillas != paste0(lista, "ctl00")]
  etiquetas <- purrr::map_chr(casillas, \(casilla) {
    html_text(html_element(pagina, sprintf("label[for='%s']", gsub("$", "_", casilla, fixed = TRUE))))
  })
  elegidas <- if (solo_total) which(stringr::str_squish(etiquetas) == "Total") else seq_along(casillas)
  if (length(elegidas) == 0) stop("No se encontró la opción Total en Municipio.")

  campos <- moca_campos(pagina)
  campos[[paste0(lista, "ctl01$HiddenIndices")]] <- paste(elegidas - 1, collapse = ",")
  for (casilla in casillas[elegidas]) campos[[casilla]] <- "on"
  campos[[paste0(moca_prefijo, "ctl04$ctl00")]] <- "View Report"
  pagina <- moca_postback(cookies, campos)

  exportar <- stringr::str_match(as.character(pagina), '"ExportUrlBase":"([^"]*)"')[, 2]
  if (is.na(exportar)) stop("El reporte no generó enlace de exportación.")
  exportar <- gsub("\\u0026", "&", exportar, fixed = TRUE)

  request(paste0("https://rssvr2.sispro.gov.co", exportar, "CSV")) |>
    req_user_agent("MSD_application (https://github.com/jujgomezru/MSD_application)") |>
    req_cookie_preserve(cookies) |>
    req_timeout(600) |>
    req_retry(max_tries = 3) |>
    req_perform(path = destino)
  invisible(destino)
}

# Descarga varias combinaciones a data/raw/moca/<indicador>_<depto>.csv.
# Salta los archivos que ya existen, así que se puede interrumpir y reanudar.
moca_descargar_todo <- function(indicadores = moca_indicadores,
                                departamentos = NULL,
                                carpeta = here::here("data", "raw", "moca"),
                                solo_total = TRUE,
                                pausa = 2) {
  dir.create(carpeta, recursive = TRUE, showWarnings = FALSE)
  if (is.null(departamentos)) departamentos <- moca_departamentos()$codigo

  for (nombre in names(indicadores)) {
    for (depto in departamentos) {
      destino <- file.path(carpeta, sprintf("%s_%s.csv", nombre, depto))
      if (file.exists(destino)) next
      message(sprintf("Descargando %s, departamento %s...", nombre, depto))
      tryCatch(
        moca_descargar(indicadores[[nombre]], depto, destino, solo_total),
        error = function(e) message("  Error: ", conditionMessage(e))
      )
      Sys.sleep(pausa)
    }
  }
}

# Lectura -------------------------------------------------------------------------

# Lee y une los CSV descargados. Los números vienen en formato colombiano
# (punto de miles, coma decimal) y las 3 primeras líneas son metadatos.
# `nivel` distingue el total país (cod_depto 170), el total departamental y las
# filas por IPS. El reporte no trae totales municipales: se calculan sumando
# numerador y denominador de las IPS del municipio.
moca_leer <- function(carpeta = here::here("data", "raw", "moca")) {
  archivos <- list.files(carpeta, pattern = "\\.csv$", full.names = TRUE)
  numero <- function(x) readr::parse_number(x, locale = readr::locale(decimal_mark = ",", grouping_mark = "."))

  purrr::map(archivos, \(archivo) {
    readr::read_csv(archivo, skip = 3, col_types = readr::cols(.default = "c"),
                    locale = readr::locale(encoding = "UTF-8"))
  }) |>
    purrr::list_rbind() |>
    janitor::clean_names() |>
    dplyr::transmute(
      indicador = stringr::str_squish(indicador),
      nivel = dplyr::case_when(
        cod_depto == "170" ~ "pais",
        ips == "Total" ~ "departamento",
        .default = "ips"
      ),
      cod_depto, departamento = stringr::str_remove(departamento, "^\\d+ - "),
      cod_municipio, municipio = stringr::str_remove(municipio, "^\\d+ - "),
      cod_ips = dplyr::na_if(stringr::str_extract(ips, "^\\d+"), ""),
      ips = stringr::str_remove(ips, "^\\d+ - "),
      fecha = as.Date(fecha, format = "%Y%m%d"),
      numerador = numero(numerador),
      denominador = numero(denominador),
      dias_promedio = numero(textbox11)
    ) |>
    dplyr::distinct()
}
