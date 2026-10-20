source(here::here("R", "setup.R"))
source(here("R", "poblacion.R"))
source(here("R", "afiliados.R"))

citas <- read_csv(here("data", "processed", "reclamos_citas.csv.gz"),
                  col_types = cols(.default = "c", n = "i")) |>
  mutate(
    fecha = suppressWarnings(make_date(as.integer(anio), as.integer(mes), 1)),
    motivo = str_squish(motivo_especifico),
    nivel = case_when(
      corte == "post" & str_detect(tipo_motivo, "^ESPECIALIDADES M") ~ "Especialista",
      corte == "post" & tipo_motivo == "MEDICINA GENERAL" ~ "Medicina general",
      corte == "pre" & str_detect(motivo, "ESPECIALIZAD") ~ "Especialista",
      corte == "pre" & str_detect(motivo, "MÉDICA GENERAL") ~ "Medicina general",
      .default = "Otros servicios"
    ),
    tipo = case_when(
      str_detect(motivo, "NEGACI") ~ "Negación de la cita",
      str_detect(motivo, "AUTORIZACI") ~ "Demora en la autorización",
      str_detect(motivo, "OPORTUNIDAD") ~ "Demora en la cita",
      .default = "Otros"
    )
  )

citas |>
  filter(nivel == "Especialista", !is.na(fecha)) |>
  count(corte, fecha, tipo, wt = n) |>
  write_csv(here("data", "processed", "citas_reclamos_mensual.csv"))

especialidades_observatorio <- c("MEDICINA INTERNA", "PEDIATRÍA", "GINECOLOGÍA Y OBSTETRICIA",
                                 "CIRUGÍA GENERAL", "GINECOLOGÍA", "OBSTETRICIA")
citas |>
  filter(corte == "post", nivel == "Especialista", tipo %in% c("Negación de la cita", "Demora en la cita")) |>
  count(especialidad = str_squish(subtipo_motivo), tipo, wt = n) |>
  filter(!is.na(especialidad), especialidad != "NULL") |>
  mutate(
    medida_observatorio = especialidad %in% especialidades_observatorio,
    especialidad = str_to_sentence(especialidad)
  ) |>
  write_csv(here("data", "processed", "citas_reclamos_especialidad.csv"))

poblacion <- dane_poblacion() |> filter(anio == 2025) |> select(cod_depto, departamento, poblacion)
espera <- read_csv(here("data", "processed", "mapa_espera_departamentos.csv"),
                   col_types = cols(cod_depto = "c")) |>
  filter(especialidad == "Medicina interna") |>
  select(cod_depto, dias)

citas |>
  filter(corte == "post", anio == "2025", nivel == "Especialista",
         tipo %in% c("Negación de la cita", "Demora en la cita")) |>
  count(cod_depto = str_pad(depto, 2, pad = "0"), tipo, wt = n) |>
  pivot_wider(names_from = tipo, values_from = n, values_fill = 0) |>
  inner_join(poblacion, by = "cod_depto") |>
  inner_join(espera, by = "cod_depto") |>
  transmute(
    cod_depto, departamento, poblacion, dias,
    negacion = `Negación de la cita`, demora = `Demora en la cita`,
    reclamos_100k = (negacion + demora) / poblacion * 1e5,
    pct_negacion = negacion / (negacion + demora)
  ) |>
  write_csv(here("data", "processed", "citas_reclamos_departamento.csv"))

citas |>
  filter(corte == "post", anio == "2025", nivel == "Especialista",
         tipo %in% c("Negación de la cita", "Demora en la cita"),
         str_detect(coalesce(tipo_entidad, ""), "RÉGIMEN (CONTRIBUTIVO|SUBSIDIADO)")) |>
  mutate(eps = str_squish(toupper(eps))) |>
  summarise(
    negacion = sum(n[tipo == "Negación de la cita"]),
    demora = sum(n[tipo == "Demora en la cita"]),
    .by = c(eps_codigo, eps)
  ) |>
  mutate(total = negacion + demora) |>
  reclamos_por_afiliados(c("negacion", "demora", "total")) |>
  mutate(pct_negacion = negacion / total) |>
  arrange(desc(total_10k)) |>
  write_csv(here("data", "processed", "citas_reclamos_eps.csv"))
