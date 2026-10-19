# Estado de abastecimiento de los medicamentos de referencia de cada
# enfermedad de alto costo en los listados mensuales del Invima (junio de 2023
# a enero de 2026). Requiere scripts/10_descargar_medicamentos.R. Escribe
# data/processed/med_desab_referencia_mensual.csv: por medicamento de
# referencia y listado, el peor estado entre sus presentaciones ("Sin reporte"
# si no está en seguimiento ese mes).

source(here::here("R", "setup.R"))
source(here("R", "medicamentos.R"))
source(here("R", "clasificacion_medicamentos.R"))

listados <- list.files(here("data", "raw", "invima", "desabastecimiento"), pattern = "\\.pdf$", full.names = TRUE) |>
  map(invima_leer_listado) |>
  list_rbind()

referencia <- read_csv(here("data", "referencia", "medicamentos_alto_costo_enfermedad.csv"), show_col_types = FALSE) |>
  mutate(clave = normalizar_texto(str_remove_all(str_remove(medicamento, " \\+.*$"), "\\(.*\\)")))

orden_estados <- c("Desabastecido", "En riesgo de desabastecimiento", "En monitorización", "No desabastecido", "Sin reporte")
estados <- listados |>
  mutate(clave = asignar_mas_especifico(normalizar_texto(nombre), unique(referencia$clave))) |>
  filter(!is.na(clave)) |>
  summarise(estado = orden_estados[min(match(estado, orden_estados))], .by = c(clave, fecha))

referencia |>
  cross_join(distinct(listados, fecha)) |>
  left_join(estados, by = c("clave", "fecha")) |>
  mutate(estado = coalesce(estado, "Sin reporte")) |>
  select(enfermedad, medicamento, tipo, fecha, estado) |>
  arrange(enfermedad, medicamento, fecha) |>
  write_csv(here("data", "processed", "med_desab_referencia_mensual.csv"))

listados |> count(fecha, estado) |> pivot_wider(names_from = estado, values_from = n, values_fill = 0) |> print(n = Inf)
