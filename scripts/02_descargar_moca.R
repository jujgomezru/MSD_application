# Descarga los tiempos de espera para asignación de citas del Observatorio
# Nacional de Calidad en Salud (Res. 256 de 2016): total país y total de cada
# departamento, por indicador. Guarda un CSV por combinación en data/raw/moca/
# y el consolidado en data/processed/moca_oportunidad_citas.csv.
#
# Son 6 indicadores x 34 opciones (33 departamentos y el total país): unos 25
# minutos. Si se interrumpe, al volver a ejecutarlo continúa donde quedó.
# Para el detalle por municipio e IPS: moca_descargar_todo(solo_total = FALSE)
# en otra carpeta.

source(here::here("R", "setup.R"))
source(here("R", "moca.R"))

moca_descargar_todo(moca_indicadores)

oportunidad <- moca_leer()
write_csv(oportunidad, here("data", "processed", "moca_oportunidad_citas.csv"))

oportunidad |>
  count(indicador, nivel) |>
  print(n = Inf)
