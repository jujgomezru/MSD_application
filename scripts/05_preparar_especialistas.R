# Prepara los datos del mapa de especialistas de index.qmd:
# data/processed/mapa_especialistas_departamentos.csv, con el número de
# profesionales registrados en ReTHUS por departamento de residencia y su tasa
# por 100.000 habitantes, junto con la tasa nacional.
#
# ReTHUS (datos.gov.co, my8c-6xkk) es acumulado: cuenta los títulos registrados
# hasta su último corte (2022), incluidos profesionales que pudieron fallecer,
# retirarse o cambiar de residencia. Sirve para comparar departamentos, no como
# conteo exacto de profesionales activos.

source(here::here("R", "setup.R"))
source(here("R", "poblacion.R"))

perfiles <- c(
  "P07 - MEDICINA"                = "Médicos (pregrado)",  # todos los títulos de medicina
  "M13 - MEDICINA INTERNA"        = "Medicina interna",
  "Q01 - CIRUGÍA GENERAL"         = "Cirugía general",
  "Q04 - GINECOLOGÍA Y OBSTETRICIA" = "Ginecología y obstetricia",
  "M15 - PEDIATRÍA"               = "Pediatría"
)

# ReTHUS agregado por departamento y perfil --------------------------------------

rethus <- httr2::request("https://www.datos.gov.co/resource/my8c-6xkk.json") |>
  httr2::req_url_query(
    `$select` = "departamentoresidencia, perfilprofesional, max(a_oactoadministrativo) as ultimo, sum(numeroregistros) as registros",
    `$where` = sprintf("perfilprofesional in (%s) AND a_oactoadministrativo <= '2022'",
                       paste0("'", names(perfiles), "'", collapse = ", ")),
    `$group` = "departamentoresidencia, perfilprofesional",
    `$limit` = 5000
  ) |>
  httr2::req_timeout(300) |>
  httr2::req_perform() |>
  httr2::resp_body_json(simplifyVector = TRUE) |>
  as_tibble() |>
  transmute(
    cod_depto = str_extract(departamentoresidencia, "^\\d{2}"),
    especialidad = perfiles[perfilprofesional],
    profesionales = as.numeric(registros)
  )

write_csv(rethus, here("data", "raw", "rethus_departamento_perfil.csv"))

# Tasa por 100.000 habitantes ------------------------------------------------------

poblacion <- dane_poblacion() |> filter(anio == 2022)

# Todos los departamentos y perfiles, con 0 donde ReTHUS no registra a nadie.
tasas <- expand_grid(cod_depto = poblacion$cod_depto, especialidad = unname(perfiles)) |>
  left_join(filter(rethus, !is.na(cod_depto)), by = c("cod_depto", "especialidad")) |>
  left_join(poblacion, by = "cod_depto") |>
  mutate(
    profesionales = replace_na(profesionales, 0),
    por_100k = profesionales / poblacion * 1e5
  )

# La tasa nacional incluye a quienes no tienen departamento de residencia.
nacional <- rethus |>
  summarise(profesionales = sum(profesionales), .by = especialidad) |>
  mutate(tasa_nacional = profesionales / sum(poblacion$poblacion) * 1e5) |>
  select(especialidad, tasa_nacional)

tasas |>
  left_join(nacional, by = "especialidad") |>
  transmute(
    cod_depto, departamento, especialidad, profesionales, poblacion,
    por_100k = round(por_100k, 2), tasa_nacional = round(tasa_nacional, 2)
  ) |>
  arrange(especialidad, cod_depto) |>
  write_csv(here("data", "processed", "mapa_especialistas_departamentos.csv"))
