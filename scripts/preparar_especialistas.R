source(here::here("R", "setup.R"))
source(here("R", "poblacion.R"))

perfiles <- c(
  "P07 - MEDICINA"                = "Médicos (pregrado)",
  "M13 - MEDICINA INTERNA"        = "Medicina interna",
  "Q01 - CIRUGÍA GENERAL"         = "Cirugía general",
  "Q04 - GINECOLOGÍA Y OBSTETRICIA" = "Ginecología y obstetricia",
  "M15 - PEDIATRÍA"               = "Pediatría"
)

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

poblacion <- dane_poblacion() |> filter(anio == 2022)

tasas <- expand_grid(cod_depto = poblacion$cod_depto, especialidad = unname(perfiles)) |>
  left_join(filter(rethus, !is.na(cod_depto)), by = c("cod_depto", "especialidad")) |>
  left_join(poblacion, by = "cod_depto") |>
  mutate(
    profesionales = replace_na(profesionales, 0),
    por_100k = profesionales / poblacion * 1e5
  )

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
