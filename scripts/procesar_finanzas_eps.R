source(here::here("R", "setup.R"))
source(here("R", "eps.R"))
source(here("R", "supersalud.R"))

supersalud_descargar_financiera()
archivos <- list.files(here("data", "raw", "supersalud_financiera"), full.names = TRUE)

cuentas <- bind_rows(
  tibble(formato = "niif", cuenta = c("ingresos", "costos", "patrimonio", "deuda", "deuda", "deuda", "deuda"),
         codigo = c("41", "61", "3", "210802", "210808", "240601", "240605")),
  tibble(formato = "publico", cuenta = c("ingresos", "costos", "patrimonio", "deuda", "deuda", "deuda"),
         codigo = c("43", "56", "3", "24", "273201", "273207"))
)

finanzas <- map(archivos, \(archivo) {
  readxl::read_excel(archivo, col_types = "text") |>
    janitor::clean_names() |>
    mutate(formato = if_else(catalogo == "FT001-07", "publico", "niif")) |>
    inner_join(cuentas, by = c("formato", "codigo_concepto" = "codigo")) |>
    transmute(
      nit, razon_social, formato, cuenta,
      valor = as.numeric(valor),
      corte = as.Date(paste0(str_extract(basename(archivo), "\\d{4}_\\d{2}"), "_01"), "%Y_%m_%d")
    )
}) |>
  list_rbind() |>
  summarise(valor = sum(valor, na.rm = TRUE), .by = c(nit, razon_social, formato, corte, cuenta)) |>
  pivot_wider(names_from = cuenta, values_from = valor, values_fill = 0) |>
  mutate(
    eps = eps_canonico(razon_social),
    meses = month(corte),
    siniestralidad = costos / ingresos,
    deuda_prestadores = deuda,
    meses_deuda = deuda_prestadores / (costos / meses)
  ) |>
  select(eps, nit, razon_social, formato, corte, ingresos, costos, siniestralidad, patrimonio,
         deuda_prestadores, meses_deuda) |>
  arrange(eps, corte)

write_csv(finanzas, here("data", "processed", "eps_finanzas.csv"), na = "")

finanzas |>
  filter(is.na(eps)) |>
  distinct(razon_social) |>
  print()
