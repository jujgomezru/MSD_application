# Bases de reclamos en salud de la Superintendencia Nacional de Salud, publicadas
# en su portal de datos abiertos (https://mapas.supersalud.gov.co/arcgisportal).
# Un registro por reclamo, con motivo, EPS, departamento del afectado y otras
# características.
#
# El 1 de julio de 2023 la Supersalud cambió la clasificación de motivos y de
# riesgo; según la entidad, no son homologables. Por eso los archivos se separan
# en dos periodos.

# `comillas`: si el archivo usa comillas para encerrar campos que contienen el
# separador (TRUE) o si hay que ignorarlas (FALSE). En los segundos no hay
# separadores dentro de los campos, pero algunos nombres de EPS traen comillas
# mal formadas (""CCF ORIENTE """"COMFAORIENTE"""""") que rompen la lectura.
supersalud_reclamos <- tibble::tribble(
  ~periodo,    ~corte,     ~id,                                  ~archivo,               ~comillas,
  "2017",      "pre",      "9d500fecea6e448aaafc656762b93ce5",   "reclamos_2017.csv",    FALSE,
  "2018",      "pre",      "9e2437ad37fb4ad58b6d7d13c284325d",   "reclamos_2018.csv",    FALSE,
  "2019",      "pre",      "ae1d1380069e41cfbedbd1e9f3e400fe",   "reclamos_2019.csv",    FALSE,
  "2020",      "pre",      "a39c6c89617e425e8947c867a9a1af66",   "reclamos_2020.csv",    FALSE,
  "2021",      "pre",      "3824e636c1b748269364c0e57c680d58",   "reclamos_2021.csv",    FALSE,
  "2022",      "pre",      "daa8a0715f284373a7a59f6040322ef2",   "reclamos_2022.csv",    FALSE,
  "2023-1",    "pre",      "01de487badc44f43862527b4f6c6b358",   "reclamos_2023_1.csv",  FALSE,
  "2023-2",    "post",     "515d4c6366854f549b24c704320f10f4",   "reclamos_2023_2.csv",  TRUE,
  "2024-1",    "post",     "b52330a16b3940c39d38cb164c9dc014",   "reclamos_2024_1.csv",  TRUE,
  "2024-2",    "post",     "b257c9907b754b0cabb55a32e706bdee",   "reclamos_2024_2.csv",  TRUE,
  "2025-1",    "post",     "289c737231d14a968224150c585d08a3",   "reclamos_2025_1.csv",  FALSE,
  "2025-2",    "post",     "d36192e86d874296aeaf95cd1a53e904",   "reclamos_2025_2.zip",  FALSE
)

# Nombres unificados de columnas: antes del corte vienen en mayúsculas (sin tipo
# ni subtipo de motivo) y después, en minúsculas.
supersalud_columnas <- c(
  anio = "AÑO|periodo",
  mes = "MES|mes",
  depto = "AFEC_COD_DEPTO|afec_cod_depto",
  regimen = "AFEC_REGAFILIACION|afec_regafiliacion",
  edad = "AFEC_EDADR|afec_edadr",
  sexo = "AFEC_GENERO|afec_genero",
  educacion = "AFEC_EDUC|afec_educ",
  etnia = "AFEC_GETNICO|afec_getnico",
  poblacion_especial = "AFEC_POBESPECIAL|afec_pobespecial",
  tipo_entidad = "ENT_TIPOVIG_SNS|ent_tipovig_sns",
  eps_codigo = "ENT_COD_SNS|ent_cod_sns",
  eps = "ENT_ALIAS_SNS|ent_alias_sn",
  macromotivo = "MACROMOTIVO|macromotivo",
  motivo_general = "MOTIVO_GENERAL|motivo_general",
  motivo_especifico = "MOTIVO_ESPECIFICO|motivo_especifico",
  tipo_motivo = "tipo_de_motivo_especifico",
  subtipo_motivo = "subtipo_de_motivo_especifico",
  patologia = "PATOLOGIA_1|patologia_1",
  patologia_tipo = "PATOLOGIA_TIPO|patologia_tipo",
  cie_10 = "CIE_10|cie_10",
  alto_costo = "ALTO_COSTO|alto_costo",
  riesgo = "RIESGO_VIDA|clasificacion_de_riesgo"
)

# Recorre los archivos descargados por bloques y calcula varias agregaciones en
# una sola lectura. `agregaciones` es una lista con nombre; cada elemento tiene
# `grupos` (columnas unificadas) y opcionalmente `filtro` (función que recibe el
# bloque y devuelve un vector lógico). Devuelve una lista de tibbles con las
# columnas corte, periodo, grupos y n (número de reclamos).
supersalud_agregar <- function(agregaciones,
                               carpeta = here::here("data", "raw", "supersalud"),
                               archivos = supersalud_reclamos,
                               bloque = 250000) {
  resultados <- purrr::map(agregaciones, \(a) list())

  for (i in seq_len(nrow(archivos))) {
    ruta <- file.path(carpeta, archivos$archivo[i])
    message(sprintf("Procesando %s...", archivos$periodo[i]))

    # Encabezado: define el separador (casi siempre ";", a veces ",") y las columnas.
    conexion <- if (grepl("\\.zip$", ruta)) unz(ruta, utils::unzip(ruta, list = TRUE)$Name[1]) else file(ruta)
    encabezado <- sub("^﻿", "", readLines(conexion, n = 1, encoding = "UTF-8", warn = FALSE))
    separador <- if (lengths(regmatches(encabezado, gregexpr(";", encabezado))) > 5) ";" else ","
    nombres <- strsplit(encabezado, separador, fixed = TRUE)[[1]]
    presentes <- purrr::map_chr(supersalud_columnas, \(patron) {
      coincide <- nombres[grepl(paste0("^(", patron, ")$"), nombres)]
      if (length(coincide) == 0) NA_character_ else coincide[1]
    })
    presentes <- presentes[!is.na(presentes)]
    tipos <- do.call(readr::cols_only, stats::setNames(rep(list(readr::col_character()), length(presentes)), presentes))

    procesar <- function(datos, posicion) {
      datos <- dplyr::rename(datos, dplyr::all_of(presentes))
      for (faltante in setdiff(names(supersalud_columnas), names(datos))) datos[[faltante]] <- NA_character_
      # Unifica variantes de escritura entre años ("SALUD; Y OTROS" / "SALUD Y OTROS").
      datos <- dplyr::mutate(datos, dplyr::across(dplyr::everything(), \(x) stringr::str_squish(gsub(";", "", x, fixed = TRUE))))
      purrr::map(agregaciones, \(a) {
        if (!is.null(a$filtro)) datos <- datos[a$filtro(datos) %in% TRUE, , drop = FALSE]
        dplyr::count(datos, dplyr::across(dplyr::all_of(a$grupos)))
      })
    }

    partes <- readr::read_delim_chunked(
      ruta,
      callback = readr::ListCallback$new(procesar),
      chunk_size = bloque, delim = separador, col_types = tipos,
      quote = if (archivos$comillas[i]) "\"" else "",
      locale = readr::locale(encoding = "UTF-8"), progress = FALSE
    )

    for (nombre in names(agregaciones)) {
      resultados[[nombre]][[i]] <- purrr::map(partes, nombre) |>
        purrr::list_rbind() |>
        dplyr::count(dplyr::across(dplyr::all_of(agregaciones[[nombre]]$grupos)), wt = n) |>
        dplyr::mutate(corte = archivos$corte[i], periodo = archivos$periodo[i], .before = 1)
    }
  }

  purrr::map(resultados, purrr::list_rbind)
}

# Descarga los archivos que falten a data/raw/supersalud/. Usa el enlace de
# descarga del portal (el servicio de consultas bloquea el acceso automatizado).
supersalud_descargar <- function(carpeta = here::here("data", "raw", "supersalud"),
                                 archivos = supersalud_reclamos) {
  dir.create(carpeta, recursive = TRUE, showWarnings = FALSE)
  for (i in seq_len(nrow(archivos))) {
    destino <- file.path(carpeta, archivos$archivo[i])
    if (file.exists(destino)) next
    message(sprintf("Descargando %s...", archivos$periodo[i]))
    temporal <- paste0(destino, ".parcial")
    httr2::request("https://mapas.supersalud.gov.co/arcgisportal/sharing/rest/content/items") |>
      httr2::req_url_path_append(archivos$id[i], "data") |>
      httr2::req_timeout(3600) |>
      httr2::req_retry(max_tries = 3) |>
      httr2::req_perform(path = temporal)
    # Solo se renombra al terminar, para no confundir una descarga cortada con una completa.
    file.rename(temporal, destino)
  }
}
