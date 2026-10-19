# Procesa los reclamos en salud de la Supersalud para el análisis de citas
# (index.qmd). Requiere haber ejecutado scripts/07_descargar_reclamos.R.
#
# Genera en data/processed/:
# - reclamos_totales.csv.gz: todos los reclamos por mes, departamento del
#   afectado, EPS, régimen y macromotivo (tendencia general y ranking de EPS).
# - reclamos_citas.csv.gz: reclamos cuyo motivo menciona citas o consultas, con
#   el detalle del motivo (demora o negación, especialidad) y el riesgo.
# - reclamos_citas_edad.csv.gz: los mismos reclamos por año, motivo y edad.
#
# Los motivos se clasifican distinto antes y después de julio de 2023 (columna
# corte); la clasificación en categorías de análisis se hace en la preparación
# de cada gráfica.

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
