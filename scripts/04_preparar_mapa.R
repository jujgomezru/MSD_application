# Prepara los datos del mapa interactivo de index.qmd:
# - data/geo/departamentos.geojson: límites departamentales del Marco
#   Geoestadístico Nacional 2025 del DANE, simplificados (unos 130 KB).
# - data/processed/mapa_espera_departamentos.csv: días promedio de espera por
#   departamento y especialidad en los últimos 12 meses, con el promedio nacional.
# Requiere haber ejecutado scripts/02_descargar_moca.R.

source(here::here("R", "setup.R"))

# Límites departamentales ---------------------------------------------------------

geo <- here("data", "geo", "departamentos.geojson")
if (!file.exists(geo)) {
  dir.create(dirname(geo), showWarnings = FALSE)
  httr2::request("https://geoportal.dane.gov.co/mparcgis/rest/services/Divipola/Serv_DIVIPOLA_MGN_2025/FeatureServer/319/query") |>
    httr2::req_url_query(
      where = "1=1",
      outFields = "DPTO_CCDGO,DPTO_CNMBRE",
      returnGeometry = "true",
      maxAllowableOffset = 0.01,  # simplificación en grados (~1 km)
      geometryPrecision = 4,
      outSR = 4326,
      f = "geojson"
    ) |>
    httr2::req_timeout(300) |>
    httr2::req_perform(path = geo) |>
    invisible()

  # El DANE entrega los anillos en sentido antihorario (estándar GeoJSON), pero
  # d3/Observable Plot los espera en sentido horario: sin invertirlos, cada
  # departamento se dibuja como el mundo entero menos el departamento.
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

# Días de espera por departamento -------------------------------------------------

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

# Últimos cuatro trimestres, ponderados por número de citas (como en las barras).
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
