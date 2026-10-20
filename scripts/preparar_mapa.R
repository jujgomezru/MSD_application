source(here::here("R", "setup.R"))

geo <- here("data", "geo", "departamentos.geojson")
if (!file.exists(geo)) {
  dir.create(dirname(geo), showWarnings = FALSE)
  httr2::request("https://geoportal.dane.gov.co/mparcgis/rest/services/Divipola/Serv_DIVIPOLA_MGN_2025/FeatureServer/319/query") |>
    httr2::req_url_query(
      where = "1=1",
      outFields = "DPTO_CCDGO,DPTO_CNMBRE",
      returnGeometry = "true",
      maxAllowableOffset = 0.01,
      geometryPrecision = 4,
      outSR = 4326,
      f = "geojson"
    ) |>
    httr2::req_timeout(300) |>
    httr2::req_perform(path = geo) |>
    invisible()

  invertir <- function(poligono) lapply(poligono, rev)
  capa <- jsonlite::read_json(geo)
  capa$features <- lapply(capa$features, \(f) {
    f$geometry$coordinates <- switch(
      f$geometry$type,
      Polygon = invertir(f$geometry$coordinates),
      MultiPolygon = lapply(f$geometry$coordinates, invertir)
    )
    f
  })
  jsonlite::write_json(capa, geo, auto_unbox = TRUE, digits = NA)
}

nombres_especialidad <- c(
  "Medicina General" = "Medicina general",
  "Medicina Interna" = "Medicina interna",
  "Pediatría"        = "Pediatría",
  "Ginecología"      = "Ginecología",
  "Obstetricia"      = "Obstetricia",
  "Cirugía General"  = "Cirugía general"
)

oportunidad <- read_csv(here("data", "processed", "moca_oportunidad_citas.csv"),
                        col_types = cols(cod_depto = "c", .default = col_guess())) |>
  mutate(especialidad = recode(str_remove(indicador, "^.*cita de "), !!!nombres_especialidad))

hasta <- max(oportunidad$fecha)
ultimo_anio <- oportunidad |>
  filter(nivel %in% c("pais", "departamento"), fecha > hasta - months(12)) |>
  summarise(
    dias = sum(numerador, na.rm = TRUE) / sum(denominador, na.rm = TRUE),
    citas = sum(denominador, na.rm = TRUE),
    .by = c(nivel, cod_depto, departamento, especialidad)
  ) |>
  mutate(dias = if_else(citas > 0, dias, NA_real_))

nacional <- ultimo_anio |>
  filter(nivel == "pais") |>
  select(especialidad, promedio_nacional = dias)

mapa <- ultimo_anio |>
  filter(nivel == "departamento") |>
  left_join(nacional, by = "especialidad") |>
  transmute(
    cod_depto, departamento, especialidad,
    dias = round(dias, 2), citas, promedio_nacional = round(promedio_nacional, 2),
    desde = floor_date(hasta - months(11), "month"), hasta
  ) |>
  arrange(especialidad, cod_depto)

write_csv(mapa, here("data", "processed", "mapa_espera_departamentos.csv"), na = "")
