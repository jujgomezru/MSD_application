source(here::here("R", "setup.R"))
source(here("R", "moca.R"))

moca_descargar_todo(moca_indicadores)

oportunidad <- moca_leer()
write_csv(oportunidad, here("data", "processed", "moca_oportunidad_citas.csv"))

oportunidad |>
  count(indicador, nivel) |>
  print(n = Inf)
