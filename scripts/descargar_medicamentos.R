source(here::here("R", "setup.R"))
source(here("R", "medicamentos.R"))

descargar_datos_gov_csv("sdmr-tfmf", here("data", "raw", "invima", "vitales_no_disponibles.csv"))
descargar_datos_gov_csv("nauz-qkjw", here("data", "raw", "cnpmdm", "precio_maximo_venta.csv"))
invima_descargar_desabastecimiento()

list.files(here("data", "raw"), recursive = TRUE, pattern = "\\.(csv|pdf)$") |>
  str_subset("invima|cnpmdm") |>
  print()
