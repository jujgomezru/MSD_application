# Contrasta hipótesis sobre la negación de citas con especialista a nivel de EPS.
# Requiere scripts/08, 12 y 16. Escribe en data/processed/:
# - eps_hipotesis_2025.csv: por EPS, negaciones por 10.000 afiliados en 2025 e
#   indicadores financieros de diciembre de 2025 (contención de costos y deudas
#   con prestadores).
# - eps_evento_intervencion.csv: estudio de evento. Reclamos mensuales por
#   negación de citas con especialista de cada EPS intervenida, como índice
#   respecto a su promedio antes de la intervención (=1), y el mismo índice para
#   el grupo de EPS no intervenidas en las mismas fechas.
# - eps_evento_resumen.csv: cambio promedio del índice en los 12 meses
#   siguientes a la intervención, para la EPS y para el grupo de control.

source(here::here("R", "setup.R"))
source(here("R", "eps.R"))

# Comparación entre EPS (2025) ----------------------------------------------------

finanzas_2025 <- read_csv(here("data", "processed", "eps_finanzas.csv"), show_col_types = FALSE) |>
  filter(corte == as.Date("2025-12-01"), ingresos > 0)

reclamos_eps <- read_csv(here("data", "processed", "citas_reclamos_eps.csv"), show_col_types = FALSE) |>
  mutate(eps_c = eps_canonico(eps))

hipotesis <- reclamos_eps |>
  inner_join(finanzas_2025, by = c("eps_c" = "eps"), suffix = c("", "_fin")) |>
  transmute(
    eps = eps_c, afiliados, negacion_10k, demora_10k, total_10k, pct_negacion,
    siniestralidad, patrimonio, patrimonio_por_afiliado = patrimonio / afiliados,
    deuda_prestadores, meses_deuda, formato
  )
write_csv(hipotesis, here("data", "processed", "eps_hipotesis_2025.csv"))

# Correlaciones de rango (Spearman) entre EPS con al menos 200.000 afiliados.
# La deuda solo se compara entre EPS con formato NIIF (la del formato público no
# es equivalente).
grandes <- filter(hipotesis, afiliados >= 2e5)
correlaciones <- tibble(
  indicador = c("Siniestralidad", "Patrimonio por afiliado", "Meses de costo adeudados a prestadores"),
  rho = c(
    cor(grandes$negacion_10k, grandes$siniestralidad, method = "spearman"),
    cor(grandes$negacion_10k, grandes$patrimonio_por_afiliado, method = "spearman"),
    with(filter(grandes, formato == "niif"), cor(negacion_10k, meses_deuda, method = "spearman"))
  ),
  eps = c(nrow(grandes), nrow(grandes), sum(grandes$formato == "niif"))
)
print(correlaciones)

# Estudio de evento de las intervenciones -------------------------------------------

mensual <- read_csv(here("data", "processed", "reclamos_citas.csv.gz"),
                    col_types = cols(.default = "c", n = "i")) |>
  filter(corte == "post", str_detect(coalesce(tipo_motivo, ""), "^ESPECIALIDADES M"),
         motivo_especifico == "NEGACIÓN EN LA ASIGNACIÓN DE CITAS O CONSULTAS") |>
  mutate(eps = eps_canonico(eps), fecha = suppressWarnings(make_date(as.integer(anio), as.integer(mes), 1))) |>
  filter(!is.na(eps), !is.na(fecha)) |>
  count(eps, fecha, wt = n)

# Tratadas: intervenidas dentro de la serie (desde julio de 2023). Control: EPS
# grandes nunca intervenidas en el periodo (se excluyen las intervenidas antes y
# Coosalud, cuyo estado no está confirmado).
tratadas <- eps_intervenciones |> filter(fecha_intervencion >= as.Date("2023-10-01"))
excluidas <- c(eps_intervenciones$eps, "Coosalud")
control <- mensual |>
  filter(!eps %in% excluidas) |>
  summarise(n = sum(n), .by = eps) |>
  filter(n >= 2000) |>
  pull(eps)

indice <- \(serie, inicio) {
  base <- mean(serie$n[serie$fecha < inicio])
  mutate(serie, indice = n / base)
}

evento <- map(seq_len(nrow(tratadas)), \(i) {
  e <- tratadas$eps[i]
  inicio <- floor_date(tratadas$fecha_intervencion[i], "month")
  tratada <- mensual |> filter(eps == e) |> indice(inicio)
  grupo_control <- mensual |>
    filter(eps %in% control) |>
    summarise(n = sum(n), .by = fecha) |>
    arrange(fecha) |>
    indice(inicio)
  tratada |>
    select(fecha, indice) |>
    left_join(select(grupo_control, fecha, indice_control = indice), by = "fecha") |>
    mutate(eps = e, fecha_intervencion = inicio,
           mes_relativo = interval(inicio, fecha) %/% months(1))
}) |>
  list_rbind()
write_csv(evento, here("data", "processed", "eps_evento_intervencion.csv"))

resumen <- evento |>
  filter(mes_relativo >= 1, mes_relativo <= 12) |>
  summarise(
    indice_despues = mean(indice),
    indice_control_despues = mean(indice_control),
    meses_antes = min(evento$mes_relativo[evento$eps == first(eps)]) * -1,
    .by = c(eps, fecha_intervencion)
  ) |>
  mutate(diferencia = indice_despues - indice_control_despues)
write_csv(resumen, here("data", "processed", "eps_evento_resumen.csv"))
print(resumen)
cat("EPS de control:", paste(control, collapse = ", "), "\n")
