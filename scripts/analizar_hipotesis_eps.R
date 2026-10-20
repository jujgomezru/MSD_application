source(here::here("R", "setup.R"))
source(here("R", "eps.R"))

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

mensual <- read_csv(here("data", "processed", "reclamos_citas.csv.gz"),
                    col_types = cols(.default = "c", n = "i")) |>
  filter(corte == "post", str_detect(coalesce(tipo_motivo, ""), "^ESPECIALIDADES M"),
         motivo_especifico == "NEGACIÓN EN LA ASIGNACIÓN DE CITAS O CONSULTAS") |>
  mutate(eps = eps_canonico(eps), fecha = suppressWarnings(make_date(as.integer(anio), as.integer(mes), 1))) |>
  filter(!is.na(eps), !is.na(fecha)) |>
  count(eps, fecha, wt = n)

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

negacion_semestral <- read_csv(here("data", "processed", "reclamos_citas.csv.gz"),
                               col_types = cols(.default = "c", n = "i")) |>
  filter(corte == "post", str_detect(coalesce(tipo_motivo, ""), "^ESPECIALIDADES M"),
         motivo_especifico == "NEGACIÓN EN LA ASIGNACIÓN DE CITAS O CONSULTAS") |>
  mutate(eps = eps_canonico(eps)) |>
  filter(!is.na(eps)) |>
  count(eps, periodo, wt = n, name = "negaciones")

panel <- read_csv(here("data", "processed", "eps_finanzas.csv"), show_col_types = FALSE) |>
  filter(ingresos > 0, formato == "niif", !is.na(eps)) |>
  mutate(periodo = if_else(month(corte) == 6, paste0(year(corte), "-2"), paste0(year(corte) + 1, "-1"))) |>
  select(eps, corte_deuda = corte, periodo, meses_deuda, siniestralidad) |>
  inner_join(negacion_semestral, by = c("eps", "periodo")) |>
  filter(negaciones >= 50)

ee_agrupado <- \(modelo, grupo) {
  X <- model.matrix(modelo)
  e <- resid(modelo)
  pan <- solve(crossprod(X))
  carne <- Reduce(`+`, map(split(seq_along(e), grupo), \(i) tcrossprod(crossprod(X[i, , drop = FALSE], e[i]))))
  g <- n_distinct(grupo)
  sqrt(diag(pan %*% carne %*% pan * g / (g - 1) * (nrow(X) - 1) / (nrow(X) - ncol(X))))
}
estimar <- \(datos, x) {
  modelo <- lm(reformulate(c(x, "factor(eps)", "factor(periodo)"), "log(negaciones)"), datos)
  b <- unname(coef(modelo)[x])
  ee <- unname(ee_agrupado(modelo, datos$eps)[x])
  tibble(indicador = x, coef = b, ee, cambio_pct = exp(b) - 1,
         ic_bajo = exp(b - 1.96 * ee) - 1, ic_alto = exp(b + 1.96 * ee) - 1,
         observaciones = nrow(datos), eps = n_distinct(datos$eps))
}
resumen_panel <- bind_rows(estimar(panel, "meses_deuda"), estimar(panel, "siniestralidad")) |>
  mutate(sd_dentro = c(sd(panel$meses_deuda - ave(panel$meses_deuda, panel$eps)),
                       sd(panel$siniestralidad - ave(panel$siniestralidad, panel$eps))))
write_csv(resumen_panel, here("data", "processed", "eps_panel_resumen.csv"))
print(resumen_panel)

residuo <- \(f) resid(lm(f, panel))
panel |>
  mutate(deuda_res = residuo(meses_deuda ~ factor(eps) + factor(periodo)),
         negacion_res = exp(residuo(log(negaciones) ~ factor(eps) + factor(periodo))) - 1) |>
  write_csv(here("data", "processed", "eps_panel_deuda.csv"))
