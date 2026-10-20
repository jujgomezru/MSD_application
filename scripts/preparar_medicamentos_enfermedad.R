source(here::here("R", "setup.R"))
source(here("R", "clasificacion_medicamentos.R"))

referencia <- read_csv(here("data", "referencia", "medicamentos_alto_costo_enfermedad.csv"), show_col_types = FALSE)

termometro <- read_csv(here("data", "raw", "minsalud", "termometro_precios_2024.csv"), show_col_types = FALSE)
precios <- precio_institucional(unique(referencia$medicamento), termometro) |>
  select(medicamento = nombre, principio_activo, unidad, precio_mediana, precio_min, precio_max)

regulados <- read_csv(here("data", "raw", "cnpmdm", "precio_maximo_venta.csv"), show_col_types = FALSE) |>
  pull(mercado_relevante) |>
  normalizar_texto()

orden_estados <- c("Desabastecido", "En riesgo de desabastecimiento", "En monitorización", "No desabastecido")
desabastecimiento <- read_csv(here("data", "processed", "med_desabastecimiento.csv"), show_col_types = FALSE) |>
  mutate(nombre = normalizar_texto(medicamento))

vitales <- read_csv(here("data", "raw", "invima", "vitales_no_disponibles.csv"), show_col_types = FALSE) |>
  filter(year(fecha_de_autorizaci_n) >= 2023) |>
  transmute(principio = normalizar_texto(paste(principio_activo1, principio_activo2)))

referencia <- mutate(referencia, clave = normalizar_texto(str_remove_all(str_remove(medicamento, " \\+.*$"), "\\(.*\\)")))
desabastecimiento <- mutate(desabastecimiento, clave = asignar_mas_especifico(nombre, unique(referencia$clave)))
vitales <- mutate(vitales, clave = asignar_mas_especifico(principio, unique(referencia$clave)))

medicamentos <- referencia |>
  mutate(
    estado = map_chr(clave, \(k) {
      e <- desabastecimiento$estado[desabastecimiento$clave %in% k]
      if (length(e) == 0) "Sin reporte" else orden_estados[min(match(e, orden_estados))]
    }),
    vitales_desde_2023 = map_int(clave, \(k) sum(vitales$clave %in% k)),
    precio_regulado = map_lgl(clave, \(k) any(str_detect(regulados, fixed(k))))
  ) |>
  left_join(precios, by = "medicamento") |>
  mutate(across(c(unidad, precio_mediana, precio_min, precio_max),
                \(x) if_else(str_detect(normalizar_texto(coalesce(principio_activo, "")), fixed(clave)), x, NA))) |>
  mutate(
    caro = !is.na(precio_mediana) & precio_mediana >= umbral_caro,
    problema = case_when(
      estado == "Desabastecido" ~ 1,
      estado == "En riesgo de desabastecimiento" ~ 2,
      vitales_desde_2023 >= 10 ~ 3,
      estado == "En monitorización" ~ 4,
      .default = NA
    ),
    motivo_cuello = case_when(
      problema == 1 ~ "Desabastecido (Invima)",
      problema == 2 ~ "En riesgo de desabastecimiento (Invima)",
      problema == 3 ~ paste(vitales_desde_2023, "importaciones como vital no disponible desde 2023"),
      problema == 4 ~ "En monitorización por riesgo de abastecimiento (Invima)",
      .default = "Mayor precio por unidad entre los de la enfermedad"
    )
  ) |>
  arrange(enfermedad, coalesce(problema, 99), desc(precio_mediana)) |>
  mutate(cuello_botella = row_number() == 1, .by = enfermedad) |>
  select(-clave)

write_csv(medicamentos, here("data", "processed", "med_enfermedad_medicamentos.csv"), na = "")

patron_enfermedad <- c(
  "Artritis reumatoide" = "ARTRITIS REUMATOIDEA",
  "Enfermedad renal y trasplante" = "^IRC|RENAL CR[OÓ]NICA|TRASPLANTE",
  "Cáncer de mama" = "C[AÁ]NCER DE MAMA",
  "VIH" = "VIH",
  "Cáncer de próstata" = "PR[OÓ]STATA",
  "Leucemias" = "LEUCEMIA",
  "Cáncer de pulmón" = "C[AÁ]NCER.*PULM",
  "Cáncer colorrectal" = "COLORECTAL|COLORRECTAL"
)

reclamos <- read_csv(here("data", "processed", "reclamos_medicamentos.csv.gz"),
                     col_types = cols(.default = "c", n = "i")) |>
  filter(corte == "post") |>
  mutate(patologia = toupper(str_squish(patologia_tipo)), motivo = categoria_motivo(motivo_especifico))

resumen_reclamos <- imap(patron_enfermedad, \(patron, enfermedad) {
  r <- filter(reclamos, str_detect(patologia, patron))
  tibble(
    enfermedad = enfermedad,
    reclamos = sum(r$n),
    pct_negacion = sum(r$n[r$motivo %in% c("Negación de la entrega", "Negación de la autorización")]) / sum(r$n)
  )
}) |>
  list_rbind()

medicamentos |>
  filter(cuello_botella) |>
  select(enfermedad, medicamento, tipo, motivo_cuello, estado, vitales_desde_2023, unidad, precio_mediana) |>
  left_join(resumen_reclamos, by = "enfermedad") |>
  arrange(desc(reclamos)) |>
  write_csv(here("data", "processed", "med_enfermedad_resumen.csv"), na = "")
