# Prepara los datos de medicamentos.qmd a partir de los reclamos procesados
# (scripts/09_procesar_reclamos_medicamentos.R) y de las fuentes del Invima
# (scripts/10_descargar_medicamentos.R). Escribe en data/processed/ los archivos
# med_*.csv.
#
# Criterio de alto costo: ver R/clasificacion_medicamentos.R. Un reclamo es de
# alto costo si su patología pertenece a un grupo de alto costo o si la
# Supersalud lo marcó con una intervención de alto costo. Como la mitad de los
# reclamos no especifica patología, la cifra de alto costo es un mínimo.

source(here::here("R", "setup.R"))
source(here("R", "clasificacion_medicamentos.R"))

# Reclamos por medicamentos ------------------------------------------------------

reclamos <- read_csv(here("data", "processed", "reclamos_medicamentos.csv.gz"),
                     col_types = cols(.default = "c", n = "i")) |>
  # El filtro de texto también trae algunos reclamos solo de dispositivos médicos.
  filter(!str_detect(coalesce(tipo_motivo, ""), "^DISPOSITIVOS")) |>
  mutate(
    # Unas pocas filas no traen mes válido: quedan sin fecha y salen de la serie.
    fecha = suppressWarnings(make_date(as.integer(anio), as.integer(mes), 1)),
    grupo = grupo_patologia(patologia_tipo),
    grupo = if_else(grupo %in% grupos_alto_costo, grupo,
                    coalesce(grupo_intervencion(alto_costo), grupo)),
    alto_costo = grupo %in% c(grupos_alto_costo, "Otras intervenciones de alto costo"),
    clase = if_else(alto_costo, "Alto costo", "Otros medicamentos"),
    motivo = categoria_motivo(motivo_especifico)
  )

# Serie mensual por clase.
reclamos |>
  filter(!is.na(fecha)) |>
  count(corte, fecha, clase, wt = n) |>
  write_csv(here("data", "processed", "med_reclamos_mensual.csv"))

# Reclamos por grupo de enfermedad.
reclamos |>
  count(corte, grupo, alto_costo, wt = n) |>
  mutate(participacion = n / sum(n), .by = corte) |>
  write_csv(here("data", "processed", "med_reclamos_grupo.csv"))

alto <- filter(reclamos, alto_costo)

# Motivos de los reclamos de alto costo, por grupo.
alto |>
  count(corte, grupo, motivo, wt = n) |>
  write_csv(here("data", "processed", "med_alto_costo_motivo.csv"))

# Patologías de alto costo más frecuentes.
# Solo patologías que son de alto costo en sí mismas (no las de pacientes con una
# intervención de alto costo, p. ej. hipertensos con cirugía cardiaca).
alto |>
  filter(grupo_patologia(patologia_tipo) %in% grupos_alto_costo) |>
  count(corte, grupo, patologia = str_to_sentence(str_squish(patologia_tipo)), wt = n) |>
  slice_max(n, n = 20, by = corte) |>
  write_csv(here("data", "processed", "med_alto_costo_patologia.csv"))

# EPS: reclamos de alto costo y su peso dentro de los reclamos por medicamentos
# de cada EPS. Solo EPS (no entes territoriales ni regímenes especiales).
reclamos |>
  filter(str_detect(coalesce(tipo_entidad, ""), "RÉGIMEN (CONTRIBUTIVO|SUBSIDIADO)")) |>
  # Una EPS puede tener un código por régimen (p. ej. EPS037 y EPSS37 para la
  # Nueva EPS): se agrupa por nombre.
  mutate(eps = str_squish(toupper(eps))) |>
  summarise(
    alto_costo = sum(n[alto_costo]),
    medicamentos = sum(n),
    .by = c(corte, eps)
  ) |>
  mutate(participacion_alto_costo = alto_costo / medicamentos) |>
  arrange(corte, desc(alto_costo)) |>
  write_csv(here("data", "processed", "med_alto_costo_eps.csv"))

# Desabastecimiento (Invima, listado de agosto de 2026) ---------------------------
# Tomado del conjunto de datos de la Secretaría Distrital de Salud de Bogotá,
# que transcribe el listado del Invima y agrega grupo terapéutico y prioridad.

read_delim(here("data", "raw", "invima", "desabastecimiento_saludata.csv"),
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
  ) |>
  write_csv(here("data", "processed", "med_desabastecimiento.csv"), na = "")

# Medicamentos vitales no disponibles (Invima) ---------------------------------------

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

# Principios activos más autorizados desde 2023, con su diagnóstico más frecuente.
vitales |>
  filter(anio >= 2023) |>
  summarise(
    autorizaciones = n(),
    diagnostico = names(sort(table(diagnostico), decreasing = TRUE))[1],
    .by = principio_activo
  ) |>
  slice_max(autorizaciones, n = 20) |>
  write_csv(here("data", "processed", "med_vitales_top.csv"))
