# Indicadores financieros por EPS a partir del catálogo de información
# financiera de la Supersalud (formato FT001, datos abiertos), cortes de junio y
# diciembre de 2021 a 2026. Escribe data/processed/eps_finanzas.csv.
#
# Los valores de ingresos y costos son acumulados en el año hasta el corte
# (6 meses en junio, 12 en diciembre). Indicadores:
# - siniestralidad: costos por atención en salud (61) / ingresos del SGSSS (41).
# - patrimonio (3).
# - deuda_prestadores: proveedores prestadores de servicios de salud (210802) +
#   reservas técnicas liquidadas pendientes de pago por servicios de salud
#   (210808) + obligaciones conocidas no liquidadas por servicios de salud
#   (240601) + glosas sobre servicios de salud (240605). Ver abajo las cuentas
#   equivalentes en el formato del sector público.
# - meses_deuda: deuda con prestadores dividida por el costo mensual promedio.

source(here::here("R", "setup.R"))
source(here("R", "eps.R"))
source(here("R", "supersalud.R"))

supersalud_descargar_financiera()
archivos <- list.files(here("data", "raw", "supersalud_financiera"), full.names = TRUE)

# Cada EPS reporta en uno de tres formatos: FT001-01 y FT001-02 (NIIF, mismos
# códigos) y FT001-07 (plan contable del sector público, que usan EPS públicas e
# indígenas). En el FT001-07 la deuda con prestadores es más gruesa: incluye
# todas las cuentas por pagar (24) además de las reservas técnicas conocidas no
# liquidadas por servicios de salud.
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
