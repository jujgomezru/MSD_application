source(here::here("R", "setup.R"))
source(here("R", "supersalud.R"))

es_cita <- \(d) grepl("CITA|CONSULTA", d$motivo_especifico)

reclamos <- supersalud_agregar(list(
  totales = list(
    grupos = c("anio", "mes", "depto", "tipo_entidad", "eps_codigo", "eps", "regimen", "macromotivo")
  ),
  citas = list(
    filtro = es_cita,
    grupos = c("anio", "mes", "depto", "tipo_entidad", "eps_codigo", "eps", "regimen",
               "motivo_general", "motivo_especifico", "tipo_motivo", "subtipo_motivo", "riesgo")
  ),
  citas_edad = list(
    filtro = es_cita,
    grupos = c("anio", "motivo_especifico", "tipo_motivo", "subtipo_motivo", "edad")
  )
))

write_csv(reclamos$totales, here("data", "processed", "reclamos_totales.csv.gz"))
write_csv(reclamos$citas, here("data", "processed", "reclamos_citas.csv.gz"))
write_csv(reclamos$citas_edad, here("data", "processed", "reclamos_citas_edad.csv.gz"))

reclamos$totales |>
  summarise(reclamos = sum(n), .by = c(corte, periodo)) |>
  left_join(summarise(reclamos$citas, citas = sum(n), .by = periodo), by = "periodo") |>
  print(n = Inf)
