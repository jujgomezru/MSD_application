source(here::here("R", "setup.R"))
source(here("R", "supersalud.R"))

es_medicamento <- \(d) grepl("MEDICAMENT", paste(d$motivo_especifico, d$tipo_motivo, d$subtipo_motivo))

reclamos <- supersalud_agregar(list(
  totales = list(
    grupos = c("anio", "mes", "depto", "eps_codigo", "eps")
  ),
  medicamentos = list(
    filtro = es_medicamento,
    grupos = c("anio", "mes", "depto", "tipo_entidad", "eps_codigo", "eps", "regimen",
               "motivo_general", "motivo_especifico", "tipo_motivo", "subtipo_motivo",
               "alto_costo", "patologia_tipo", "riesgo")
  ),
  medicamentos_patologia = list(
    filtro = es_medicamento,
    grupos = c("anio", "alto_costo", "patologia_tipo", "patologia", "cie_10")
  )
))

write_csv(reclamos$totales, here("data", "processed", "reclamos_totales_medicamentos.csv.gz"))
write_csv(reclamos$medicamentos, here("data", "processed", "reclamos_medicamentos.csv.gz"))
write_csv(reclamos$medicamentos_patologia, here("data", "processed", "reclamos_medicamentos_patologia.csv.gz"))

reclamos$totales |>
  summarise(reclamos = sum(n), .by = c(corte, periodo)) |>
  left_join(summarise(reclamos$medicamentos, medicamentos = sum(n), .by = periodo), by = "periodo") |>
  print(n = Inf)
