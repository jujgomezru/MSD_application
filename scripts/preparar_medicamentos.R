source(here::here("R", "setup.R"))
source(here("R", "clasificacion_medicamentos.R"))
source(here("R", "afiliados.R"))

reclamos <- read_csv(here("data", "processed", "reclamos_medicamentos.csv.gz"),
                     col_types = cols(.default = "c", n = "i")) |>
  filter(!str_detect(coalesce(tipo_motivo, ""), "^DISPOSITIVOS")) |>
  mutate(
    fecha = suppressWarnings(make_date(as.integer(anio), as.integer(mes), 1)),
    grupo = grupo_patologia(patologia_tipo),
    grupo = if_else(grupo %in% grupos_alto_costo, grupo,
                    coalesce(grupo_intervencion(alto_costo), grupo)),
    alto_costo = grupo %in% c(grupos_alto_costo, "Otras intervenciones de alto costo"),
    clase = if_else(alto_costo, "Alto costo", "Otros medicamentos"),
    motivo = categoria_motivo(motivo_especifico)
  )

reclamos |>
  filter(!is.na(fecha)) |>
  count(corte, fecha, clase, wt = n) |>
  write_csv(here("data", "processed", "med_reclamos_mensual.csv"))

reclamos |>
  count(corte, grupo, alto_costo, wt = n) |>
  mutate(participacion = n / sum(n), .by = corte) |>
  write_csv(here("data", "processed", "med_reclamos_grupo.csv"))

alto <- filter(reclamos, alto_costo)

alto |>
  count(corte, grupo, motivo, wt = n) |>
  write_csv(here("data", "processed", "med_alto_costo_motivo.csv"))

alto |>
  filter(grupo_patologia(patologia_tipo) %in% grupos_alto_costo) |>
  count(corte, grupo, patologia = str_to_sentence(str_squish(patologia_tipo)), wt = n) |>
  slice_max(n, n = 20, by = corte) |>
  write_csv(here("data", "processed", "med_alto_costo_patologia.csv"))

reclamos |>
  filter(anio == "2025", str_detect(coalesce(tipo_entidad, ""), "RÉGIMEN (CONTRIBUTIVO|SUBSIDIADO)")) |>
  mutate(eps = str_squish(toupper(eps))) |>
  summarise(
    alto_costo = sum(n[alto_costo]),
    medicamentos = sum(n),
    .by = c(eps_codigo, eps)
  ) |>
  reclamos_por_afiliados(c("alto_costo", "medicamentos")) |>
  arrange(desc(alto_costo_10k)) |>
  write_csv(here("data", "processed", "med_alto_costo_eps.csv"))

desabastecimiento <- read_delim(here("data", "raw", "invima", "desabastecimiento_saludata.csv"),
                                delim = ";", show_col_types = FALSE) |>
  janitor::clean_names() |>
  transmute(
    medicamento = str_squish(nombre_del_farmaco),
    estado = case_when(
      str_detect(estado, "(?i)^desaba") ~ "Desabastecido",
      str_detect(estado, "(?i)riesgo") ~ "En riesgo de desabastecimiento",
      str_detect(estado, "(?i)monitori") ~ "En monitorización",
      .default = "No desabastecido"
    ),
    causa = str_squish(str_to_sentence(causa)),
    prioridad = str_to_sentence(prioridad_en_salud),
    grupo_terapeutico = str_to_sentence(str_squish(indicaciony_o_tratamiento)),
    grupo_alto_costo = grupo_medicamento(medicamento, prioridad_en_salud, indicaciony_o_tratamiento),
    alto_costo = !is.na(grupo_alto_costo)
  )

termometro <- read_csv(here("data", "raw", "minsalud", "termometro_precios_2024.csv"), show_col_types = FALSE)
precios <- precio_institucional(unique(desabastecimiento$medicamento), termometro)

regulados <- read_csv(here("data", "raw", "cnpmdm", "precio_maximo_venta.csv"), show_col_types = FALSE) |>
  pull(mercado_relevante) |>
  str_remove(" - .*$") |>
  normalizar_texto() |>
  unique()

desabastecimiento |>
  left_join(precios, by = c("medicamento" = "nombre")) |>
  mutate(
    precio_regulado = !is.na(principio_activo) & normalizar_texto(principio_activo) %in% regulados,
    caro = !is.na(precio_mediana) & precio_mediana >= umbral_caro
  ) |>
  write_csv(here("data", "processed", "med_desabastecimiento.csv"), na = "")

vitales <- read_csv(here("data", "raw", "invima", "vitales_no_disponibles.csv"), show_col_types = FALSE) |>
  transmute(
    anio = year(fecha_de_autorizaci_n),
    tipo = str_to_sentence(tipo_de_solicitud),
    principio_activo = str_to_sentence(str_squish(principio_activo1)),
    diagnostico = str_to_sentence(str_squish(diagnostico_cie_1no_reporta)),
    cie_10 = c_digo_diagnostico_cie_10
  )

vitales |>
  count(anio, tipo) |>
  write_csv(here("data", "processed", "med_vitales_anual.csv"))

vitales |>
  filter(anio >= 2023) |>
  summarise(
    autorizaciones = n(),
    diagnostico = names(sort(table(diagnostico), decreasing = TRUE))[1],
    .by = principio_activo
  ) |>
  slice_max(autorizaciones, n = 20) |>
  write_csv(here("data", "processed", "med_vitales_top.csv"))
