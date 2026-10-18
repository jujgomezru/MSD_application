# Gráficas nacionales de tiempo de espera para asignación de citas:
# 1. Evolución en el tiempo por especialidad (línea).
# 2. Comparación de cada especialidad con medicina general (barras).
# Requiere haber ejecutado scripts/02_descargar_moca.R.

source(here::here("R", "setup.R"))

# Textos ------------------------------------------------------------------------
# Todo lo que se lee en las gráficas se edita aquí. En los textos de las barras,
# {desde}, {hasta}, {min}, {max}, {dias} y {veces} se reemplazan por los valores
# calculados.

nombres_especialidad <- c(
  "Medicina General" = "Medicina general",
  "Medicina Interna" = "Medicina interna",
  "Pediatría"        = "Pediatría",
  "Ginecología"      = "Ginecología",
  "Obstetricia"      = "Obstetricia",
  "Cirugía General"  = "Cirugía general"
)
referencia <- "Medicina general"  # debe coincidir con un valor de nombres_especialidad

textos_fuente <- paste0(
  "Fuente: Minsalud, Observatorio Nacional de Calidad en Salud (Res. 256 de 2016).\n",
  "Cortes semestrales hasta 2019 y trimestrales desde 2020. El último corte es preliminar."
)

textos_linea <- list(
  titulo    = "Tiempo de espera para asignación de cita en Colombia",
  subtitulo = "Días promedio entre la solicitud y la cita asignada, total nacional",
  eje_x     = NULL,
  eje_y     = "Días"
)

textos_barras <- list(
  titulo    = "Tiempo de espera promedio por cita de Medicina Especializada",
  subtitulo = NULL,
  eje_x     = "Días",
  eje_y     = NULL,
  etiqueta_referencia   = "{dias} días",
  etiqueta_especialidad = "{dias} días  ({veces}×)"
)

# Los hitos verticales de la gráfica de líneas se editan en R/tema.R
# (eventos_politica).

# Formatos ------------------------------------------------------------------------

meses <- c("ene", "feb", "mar", "abr", "may", "jun", "jul", "ago", "sep", "oct", "nov", "dic")
mes_anio <- function(fecha) paste(meses[month(fecha)], year(fecha))
decimal <- function(x) format(round(x, 1), nsmall = 1, decimal.mark = ",", trim = TRUE)

# Datos ---------------------------------------------------------------------------

oportunidad <- read_csv(here("data", "processed", "moca_oportunidad_citas.csv"),
                        show_col_types = FALSE)

nacional <- oportunidad |>
  filter(nivel == "pais") |>
  mutate(especialidad = recode(str_remove(indicador, "^.*cita de "), !!!nombres_especialidad))

# 1. Línea: evolución nacional -----------------------------------------------------

# Orden de la leyenda: de mayor a menor espera en el último corte.
orden <- nacional |>
  filter(fecha == max(fecha)) |>
  arrange(desc(dias_promedio)) |>
  pull(especialidad)

g_linea <- nacional |>
  mutate(especialidad = factor(especialidad, levels = orden)) |>
  ggplot(aes(fecha, dias_promedio, colour = especialidad)) +
  capa_eventos() +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.2) +
  scale_colour_informe() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.12))) +
  guides(colour = guide_legend(nrow = 1)) +
  labs(
    title = textos_linea$titulo,
    subtitle = textos_linea$subtitulo,
    x = textos_linea$eje_x, y = textos_linea$eje_y,
    caption = textos_fuente
  )

guardar_grafica(g_linea, "espera_nacional_linea", ancho = 10, alto = 5.5)

# 2. Barras: cada especialidad frente a medicina general ----------------------------

# Últimos cuatro trimestres, ponderados por número de citas. Cada corte
# trimestral cubre los tres meses que terminan en su fecha.
hasta <- max(nacional$fecha)
desde <- floor_date(hasta - months(11), "month")

ultimo_anio <- nacional |>
  filter(fecha > hasta - months(12)) |>
  summarise(
    dias = sum(numerador, na.rm = TRUE) / sum(denominador, na.rm = TRUE),
    .by = especialidad
  )

dias_referencia <- ultimo_anio$dias[ultimo_anio$especialidad == referencia]

barras <- ultimo_anio |>
  mutate(
    es_referencia = especialidad == referencia,
    veces = dias / dias_referencia,
    etiqueta = if_else(
      es_referencia,
      str_glue(textos_barras$etiqueta_referencia, dias = decimal(dias)),
      str_glue(textos_barras$etiqueta_especialidad, dias = decimal(dias), veces = decimal(veces))
    ),
    especialidad = fct_reorder(especialidad, dias)
  )

valores_titulo <- list(
  min = decimal(min(barras$veces[!barras$es_referencia])),
  max = decimal(max(barras$veces)),
  desde = mes_anio(desde),
  hasta = mes_anio(hasta)
)

g_barras <- ggplot(barras, aes(dias, especialidad, fill = es_referencia)) +
  geom_col(width = 0.65) +
  geom_text(aes(label = etiqueta), hjust = -0.05, size = 3.3,
            colour = colores_base[["texto_sec"]]) +
  scale_fill_manual(values = c(`TRUE` = colores_base[["gris_serie"]],
                               `FALSE` = paleta_categorica[["azul"]]),
                    guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.45))) +
  labs(
    title = str_glue_data(valores_titulo, textos_barras$titulo),
    subtitle = str_glue_data(valores_titulo, textos_barras$subtitulo),
    x = textos_barras$eje_x, y = textos_barras$eje_y,
    caption = textos_fuente
  ) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = colores_base[["grilla"]], linewidth = 0.3),
        axis.line.x = element_blank())

guardar_grafica(g_barras, "espera_nacional_barras", ancho = 10, alto = 5)
