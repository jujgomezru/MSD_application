# Clasificación de alto costo para medicamentos.
#
# Criterio: son de alto costo los medicamentos para las enfermedades que vigila
# la Cuenta de Alto Costo y las enfermedades huérfanas (Ley 1392 de 2010):
# cáncer, VIH, enfermedad renal crónica y trasplante, enfermedades autoinmunes y
# reumáticas, hemofilia y otras coagulopatías, y enfermedades huérfanas.

grupos_alto_costo <- c(
  "Cáncer", "VIH", "Enfermedad renal y trasplante", "Autoinmunes y reumáticas",
  "Hemofilia y coagulopatías", "Enfermedades huérfanas"
)

# Agrupa el tipo de patología de los reclamos de la Supersalud. El orden de las
# reglas importa: las enfermedades raras con nombres parecidos a las comunes
# (p. ej. hipertensión pulmonar idiopática) se evalúan antes que las comunes.
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
    # Lo demás es la larga lista de enfermedades raras del listado de huérfanas.
    .default = "Enfermedades huérfanas"
  )
}

# Intervención marcada como alto costo en el reclamo (campo alto_costo de la
# Supersalud), agrupada como las patologías. Algunos valores del campo no son de
# alto costo (p. ej. alimentos en polvo con micronutrientes) y se descartan.
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

# Categoría del motivo de un reclamo por medicamentos. Funciona con ambas
# clasificaciones de la Supersalud (antes y después de julio de 2023).
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

# Grupo de alto costo de un medicamento del listado de desabastecimiento, a
# partir de su nombre, su prioridad en salud y su grupo terapéutico.
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
