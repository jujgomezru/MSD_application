eps_canonico <- function(nombre) {
  n <- toupper(stringi::stri_trans_general(nombre, "Latin-ASCII"))
  dplyr::case_when(
    stringr::str_detect(n, "NUEVA (EPS|EMPRESA PROMOTORA)") ~ "Nueva EPS",
    stringr::str_detect(n, "SANITAS") ~ "Sanitas",
    stringr::str_detect(n, "SALUD TOTAL") ~ "Salud Total",
    stringr::str_detect(n, "SURA") ~ "Sura",
    stringr::str_detect(n, "FAMISANAR") ~ "Famisanar",
    stringr::str_detect(n, "COMPENSAR") ~ "Compensar",
    stringr::str_detect(n, "OCCIDENTAL|\\bSOS\\b") ~ "SOS",
    stringr::str_detect(n, "COOSALUD") ~ "Coosalud",
    stringr::str_detect(n, "MUTUAL SER") ~ "Mutual Ser",
    stringr::str_detect(n, "SAVIA") ~ "Savia Salud",
    stringr::str_detect(n, "ASMET") ~ "Asmet Salud",
    stringr::str_detect(n, "EMSSANAR") ~ "Emssanar",
    stringr::str_detect(n, "CAPITAL SALUD") ~ "Capital Salud",
    stringr::str_detect(n, "CAJACOPI") ~ "Cajacopi",
    stringr::str_detect(n, "COMFENALCO VALLE") ~ "Comfenalco Valle",
    stringr::str_detect(n, "ALIANSALUD|COLMEDICA") ~ "Aliansalud",
    stringr::str_detect(n, "CAPRESOCA") ~ "Capresoca",
    stringr::str_detect(n, "SALUD MIA") ~ "Salud Mía",
    stringr::str_detect(n, "COMFAORIENTE|ORIENTE") ~ "Comfaoriente",
    stringr::str_detect(n, "CHOCO") ~ "Comfachocó",
    stringr::str_detect(n, "FAMILIAR DE COLOMBIA|SUCRE") ~ "Familiar de Colombia",
    stringr::str_detect(n, "INDIGENA DEL CAUCA|\\bAIC\\b") ~ "AIC",
    stringr::str_detect(n, "DUSAKAWI") ~ "Dusakawi",
    stringr::str_detect(n, "MALLAMAS") ~ "Mallamás",
    stringr::str_detect(n, "ANAS WAYUU") ~ "Anas Wayuu",
    stringr::str_detect(n, "PIJAOS") ~ "Pijaos Salud",
    stringr::str_detect(n, "SALUD BOLIVAR") ~ "Salud Bolívar",
    stringr::str_detect(n, "CONVIDA") ~ "Convida",
    stringr::str_detect(n, "GUAJIRA") ~ "Comfaguajira",
    stringr::str_detect(n, "NARINO") ~ "Comfamiliar Nariño",
    stringr::str_detect(n, "ECOOPSOS") ~ "Ecoopsos",
    stringr::str_detect(n, "COMPARTA") ~ "Comparta",
    .default = NA_character_
  )
}

eps_intervenciones <- tibble::tribble(
  ~eps,            ~fecha_intervencion,
  "Emssanar",      "2022-02-02",
  "Asmet Salud",   "2023-05-11",
  "Savia Salud",   "2023-06-16",
  "Sanitas",       "2024-04-02",
  "Nueva EPS",     "2024-04-03",
  "SOS",           "2024-04-01",
  "Famisanar",     "2024-09-01",
  "Capresoca",     "2024-10-07"
) |>
  dplyr::mutate(fecha_intervencion = as.Date(fecha_intervencion))
