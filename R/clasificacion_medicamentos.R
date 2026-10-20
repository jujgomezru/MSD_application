grupos_alto_costo <- c(
  "Cáncer", "VIH", "Enfermedad renal y trasplante", "Autoinmunes y reumáticas",
  "Hemofilia y coagulopatías", "Enfermedades huérfanas"
)

grupo_patologia <- function(patologia_tipo) {
  p <- stringr::str_squish(toupper(patologia_tipo))
  dplyr::case_when(
    is.na(p) | p %in% c("", "NA", "NULL", "NO APLICA", "OTROS") |
      stringr::str_detect(p, "^PROBLEMAS RELACIONADOS CON FACILIDADES") ~ "Sin patología especificada",
    stringr::str_detect(p, "C[AÁ]NCER|LEUCEMIA|LINFOMA|MIELOMA|TUMOR|SARCOMA|MELANOMA|NEOPLASIA|CARCINOMA|MIELODISPL") ~ "Cáncer",
    stringr::str_detect(p, "VIH") ~ "VIH",
    stringr::str_detect(p, "^IRC|RENAL CR[OÓ]NICA|TRASPLANTE|DI[AÁ]LISIS") ~ "Enfermedad renal y trasplante",
    stringr::str_detect(p, "HEMOFILIA|WILLEBRAND|COAGULACI[OÓ]N") ~ "Hemofilia y coagulopatías",
    stringr::str_detect(p, "ARTRITIS REUMATOIDEA|LUPUS|ESPONDILITIS|PSORIASIS|ESCLEROSIS M[UÚ]LTIPLE|CROHN|COLITIS ULCERATIVA") ~ "Autoinmunes y reumáticas",
    stringr::str_detect(p, "HTA|^HIPERTENSI[OÓ]N ARTERIAL$|DIABETES$|^DIABETES|ICC|CORONARIA|CARDIOVASCULAR|ACV|OBESIDAD|SOBREPESO|FACTORES DE RIESGO") &
      !stringr::str_detect(p, "PULMONAR|NEONATAL|INS[IÍ]PIDA") ~ "Cardiovasculares y metabólicas",
    p %in% c("OTROS TRASTORNOS MENTALES", "EPILEPSIA", "NEUROLÓGICAS") |
      stringr::str_detect(p, "DEPRESI|ESQUIZOFREN|BIPOLAR|ALZHEIMER|AUTISMO|DEMENCIA$") ~ "Salud mental y neurológicas",
    stringr::str_detect(p, "PSICOL|PSIQUI|SUICIDIO") ~ "Salud mental y neurológicas",
    stringr::str_detect(p, "EPOC|ASMA|NEUMON|TBC|TUBERCULOSIS|COVID|S[IÍ]FILIS|MALARIA|DENGUE|INMUNOPREVENIBLES") ~ "Respiratorias e infecciosas",
    stringr::str_detect(p, "EMBARAZO|NIÑOS|RECI[EÉ]N NACIDO|PREMATUREZ|ADOLESCENTES|MATERNO|LACTANCIA") ~ "Materno-infantil",
    stringr::str_detect(p, "SALUD ORAL|GLAUCOMA|REEMPLAZOS ARTICULARES|CUIDADO INTENSIVO|GRAN QUEMADO|REQUIERA CIRUG|^ATENCI[OÓ]N") ~ "Otras",
    .default = "Enfermedades huérfanas"
  )
}

grupo_intervencion <- function(alto_costo) {
  a <- stringr::str_squish(toupper(alto_costo))
  dplyr::case_when(
    stringr::str_detect(a, "QUIMIOTERAPIA|RADIOTERAPIA|C[AÁ]NCER") ~ "Cáncer",
    stringr::str_detect(a, "VIH") ~ "VIH",
    stringr::str_detect(a, "TRASPLANTE|DI[AÁ]LISIS|RENAL") ~ "Enfermedad renal y trasplante",
    is.na(a) | a %in% c("", "NO APLICA") | stringr::str_detect(a, "ALIMENTO") ~ NA_character_,
    .default = "Otras intervenciones de alto costo"
  )
}

categoria_motivo <- function(motivo_especifico) {
  m <- stringr::str_squish(toupper(motivo_especifico))
  dplyr::case_when(
    stringr::str_detect(m, "REEMBOLSO") ~ "Reembolsos",
    stringr::str_detect(m, "NEGACI.*AUTORIZACI") ~ "Negación de la autorización",
    stringr::str_detect(m, "NEGACI") ~ "Negación de la entrega",
    stringr::str_detect(m, "AUTORIZACI") ~ "Demora en la autorización",
    stringr::str_detect(m, "ENTREGA|FRACCIONAMIENTO|CONTRATACI") ~ "Demora o entrega incompleta",
    stringr::str_detect(m, "MANEJO|ALMACENAMIENTO|DISPENSACI|VENCID|ERRAD|MALAS CONDICIONES") ~ "Fallas en la dispensación",
    .default = "Otros"
  )
}

antirretrovirales <- "ABACAVIR|LAMIVUDIN|ZIDOVUDIN|TENOFOVIR|EMTRICITABIN|EFAVIRENZ|NEVIRAPIN|DOLUTEGRAVIR|RALTEGRAVIR|BICTEGRAVIR|RITONAVIR|LOPINAVIR|ATAZANAVIR|DARUNAVIR|ETRAVIRIN|RILPIVIRIN"

grupo_medicamento <- function(nombre, prioridad, grupo_terapeutico) {
  n <- toupper(nombre); p <- toupper(prioridad); g <- toupper(grupo_terapeutico)
  dplyr::case_when(
    stringr::str_detect(p, "C[AÁ]NCER") ~ "Cáncer",
    stringr::str_detect(n, antirretrovirales) ~ "VIH",
    stringr::str_detect(n, "FACTOR (VII|VIII|IX)|EMICIZUMAB") ~ "Hemofilia y coagulopatías",
    stringr::str_detect(n, "POETIN") ~ "Enfermedad renal y trasplante",
    stringr::str_detect(g, "INMUNOSUPRESOR|CITOKINAS|INMUNOGLOBULINA|SUEROS INMUNOL") |
      stringr::str_detect(n, "MAB\\b|MAB |CEPT\\b|TACROLIMUS|CICLOSPORIN|MICOFENOL|AZATIOPRIN") ~ "Biológicos e inmunosupresores",
    .default = NA_character_
  )
}

normalizar_texto <- function(x) {
  x <- tolower(stringi::stri_trans_general(x, "Latin-ASCII"))
  stringr::str_squish(gsub("[^a-z0-9 ]", " ", x))
}

palabras_sal <- c("de", "acetato", "clorhidrato", "diclorhidrato", "sodico", "sodica", "sodio", "potasico",
                  "mesilato", "maleato", "fumarato", "citrato", "sulfato", "fosfato", "mofetilo", "disoproxil",
                  "monohidrato", "trihidrato", "bromuro", "succinato", "tartrato", "besilato")

asignar_mas_especifico <- function(textos, claves) {
  claves <- claves[order(-nchar(claves))]
  purrr::map_chr(textos, \(t) {
    k <- claves[stringr::str_detect(t, stringr::fixed(claves))]
    if (length(k) == 0) NA_character_ else k[1]
  })
}

precio_institucional <- function(nombres, termometro) {
  precios <- termometro |>
    dplyr::filter(canal == "Institucional", precio_por_tableta > 0) |>
    dplyr::mutate(
      principio = normalizar_texto(principio_activo),
      componentes = purrr::map(strsplit(gsub(" y | mas ", " ", principio), " "), \(cs) setdiff(cs, palabras_sal)),
      forma = dplyr::case_when(
        unidad_de_dispensacion %in% c("Tableta", "Capsula") ~ "oral solida",
        unidad_de_dispensacion %in% c("Vial", "Ampolla", "Jeringa Prellenada", "Pluma", "Bolsa") ~ "inyectable",
        .default = "otra"
      )
    ) |>
    dplyr::mutate(familia = purrr::map_chr(componentes, \(cs) if (length(cs) == 0) "" else cs[1])) |>
    dplyr::mutate(
      mediana_resto = purrr::map_dbl(dplyr::row_number(), \(i) if (dplyr::n() < 3) NA_real_ else stats::median(precio_por_tableta[-i])),
      .by = familia
    ) |>
    dplyr::filter(is.na(mediana_resto) | precio_por_tableta <= 10 * mediana_resto) |>
    dplyr::select(-mediana_resto)
  principios <- dplyr::distinct(precios, principio, componentes) |>
    dplyr::filter(lengths(componentes) > 0)

  purrr::map(nombres, \(nombre) {
    n <- stringr::str_squish(gsub("\\bsin [a-z]+", "", normalizar_texto(nombre)))
    palabras <- strsplit(n, " ")[[1]]
    coincide <- purrr::map_lgl(principios$componentes, \(cs) all(purrr::map_lgl(cs, \(c) any(startsWith(palabras, substr(c, 1, 7))))))
    candidatos <- principios[coincide, ]
    vacio <- tibble::tibble(nombre = nombre, principio_activo = NA_character_, unidad = NA_character_,
                            precio_min = NA_real_, precio_mediana = NA_real_, precio_max = NA_real_, presentaciones = 0L)
    if (nrow(candidatos) == 0) return(vacio)
    elegido <- candidatos$principio[which.max(lengths(candidatos$componentes))]
    filas <- dplyr::filter(precios, principio == elegido)
    forma <- dplyr::case_when(
      stringr::str_detect(n, "tablet|capsul") ~ "oral solida",
      stringr::str_detect(n, "inyect|vial|ampol|polvo") ~ "inyectable",
      .default = NA_character_
    )
    if (!is.na(forma) && any(filas$forma == forma)) filas <- dplyr::filter(filas, forma == !!forma)
    tibble::tibble(
      nombre = nombre,
      principio_activo = filas$principio_activo[1],
      unidad = names(sort(table(filas$unidad_de_dispensacion), decreasing = TRUE))[1],
      precio_min = min(filas$precio_por_tableta),
      precio_mediana = stats::median(filas$precio_por_tableta),
      precio_max = max(filas$precio_por_tableta),
      presentaciones = nrow(filas)
    )
  }) |>
    purrr::list_rbind()
}

umbral_caro <- 1e5
