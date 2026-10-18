# Prepara los datos del gráfico de citas per cápita de index.qmd:
# data/processed/citas_per_capita_departamentos.csv, con las citas asignadas
# por 1.000 habitantes y los días de espera por departamento y especialidad en
# los últimos 12 meses, y su cuadrante frente a los valores nacionales.
# Requiere haber ejecutado scripts/04_preparar_mapa.R.

source(here::here("R", "setup.R"))
source(here("R", "poblacion.R"))

espera <- read_csv(here("data", "processed", "mapa_espera_departamentos.csv"),
                   col_types = cols(cod_depto = "c"))

# El periodo (jul 2025 - jun 2026) abarca dos años: se promedia su población.
anios <- unique(c(year(min(espera$desde)), year(max(espera$hasta))))
poblacion <- dane_poblacion() |>
  filter(anio %in% anios) |>
  summarise(poblacion = mean(poblacion), .by = cod_depto)

nacional_citas <- espera |>
  left_join(poblacion, by = "cod_depto") |>
  summarise(citas_por_1000_nacional = sum(citas) / sum(poblacion) * 1000, .by = especialidad)

espera |>
  left_join(poblacion, by = "cod_depto") |>
  left_join(nacional_citas, by = "especialidad") |>
  mutate(
    citas_por_1000 = citas / poblacion * 1000,
    cuadrante = case_when(
      citas_por_1000 >= citas_por_1000_nacional & dias <= promedio_nacional ~ "Más citas, menos espera",
      citas_por_1000 >= citas_por_1000_nacional ~ "Más citas, más espera",
      dias <= promedio_nacional ~ "Menos citas, menos espera",
      .default = "Menos citas, más espera"
    )
  ) |>
  transmute(
    cod_depto, departamento, especialidad, citas, poblacion = round(poblacion),
    citas_por_1000 = round(citas_por_1000, 2), dias,
    citas_por_1000_nacional = round(citas_por_1000_nacional, 2),
    promedio_nacional, cuadrante, desde, hasta
  ) |>
  write_csv(here("data", "processed", "citas_per_capita_departamentos.csv"))
