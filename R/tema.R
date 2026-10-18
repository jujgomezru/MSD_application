# Estilo gráfico común para todas las figuras del informe.
# Cargar con source(here::here("R", "tema.R")) o mediante R/setup.R.

library(ggplot2)

# Colores ---------------------------------------------------------------------

# Tinta y fondo (modo claro).
colores_base <- c(
  fondo       = "#fcfcfb",
  texto       = "#0b0b0b",
  texto_sec   = "#52514e",
  texto_muted = "#898781",
  grilla      = "#e1e0d9",
  eje         = "#c3c2b7",
  gris_serie  = "#c3c2b7"  # series de contexto ("resto de EPS")
)

# Paleta categórica en orden fijo: asignar siempre en este orden, nunca ciclar.
# Validada para daltonismo (ΔE adyacente >= 9.1). Aqua, amarillo y magenta
# quedan bajo 3:1 de contraste sobre el fondo: usarlas con etiquetas directas.
# Con más de 8 categorías, agrupar en "Otras" o usar facetas.
paleta_categorica <- c(
  azul    = "#2a78d6",
  naranja = "#eb6834",
  aqua    = "#1baf7a",
  amarillo = "#eda100",
  magenta = "#e87ba4",
  verde   = "#008300",
  violeta = "#6250d6",
  rojo    = "#e34948"
)

# Rampa secuencial de un solo tono (azul), de claro a oscuro: mapas y heatmaps.
paleta_secuencial <- c(
  "#cde2fb", "#9ec5f4", "#6da7ec", "#3987e5",
  "#256abf", "#184f95", "#0d366b"
)

# Divergente: azul <-> rojo con punto medio gris neutro.
paleta_divergente <- c(bajo = "#2a78d6", medio = "#f0efec", alto = "#e34948")

# Tema ------------------------------------------------------------------------

theme_informe <- function(base_size = 12, base_family = "sans") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      plot.background  = element_rect(fill = colores_base[["fondo"]], colour = NA),
      panel.background = element_rect(fill = colores_base[["fondo"]], colour = NA),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_line(colour = colores_base[["grilla"]], linewidth = 0.3),
      axis.line.x  = element_line(colour = colores_base[["eje"]], linewidth = 0.4),
      axis.ticks   = element_blank(),
      axis.text    = element_text(colour = colores_base[["texto_muted"]], size = rel(0.85)),
      axis.title   = element_text(colour = colores_base[["texto_sec"]], size = rel(0.9)),
      plot.title   = element_text(colour = colores_base[["texto"]], face = "bold",
                                  size = rel(1.25), margin = margin(b = 4)),
      plot.subtitle = element_text(colour = colores_base[["texto_sec"]],
                                   margin = margin(b = 12)),
      plot.caption = element_text(colour = colores_base[["texto_muted"]], hjust = 0,
                                  size = rel(0.75), margin = margin(t = 10)),
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      legend.position = "top",
      legend.justification = "left",
      legend.title = element_blank(),
      legend.text  = element_text(colour = colores_base[["texto_sec"]]),
      strip.text   = element_text(colour = colores_base[["texto"]], face = "bold", hjust = 0),
      plot.margin  = margin(12, 16, 12, 12)
    )
}

# Escalas ---------------------------------------------------------------------

scale_colour_informe <- function(...) {
  scale_colour_manual(values = unname(paleta_categorica), ...)
}

scale_fill_informe <- function(...) {
  scale_fill_manual(values = unname(paleta_categorica), ...)
}

scale_fill_informe_c <- function(...) {
  scale_fill_gradientn(colours = paleta_secuencial, ...)
}

scale_fill_informe_div <- function(midpoint = 0, ...) {
  scale_fill_gradient2(low = paleta_divergente[["bajo"]],
                       mid = paleta_divergente[["medio"]],
                       high = paleta_divergente[["alto"]],
                       midpoint = midpoint, ...)
}

# Resalta una o varias categorías (p. ej. una EPS) y deja el resto en gris.
# Uso: scale_colour_manual(values = colores_destacar(niveles, "NUEVA EPS"))
# Si el resto es "contexto", es más claro agruparlo en una sola categoría
# ("Otras EPS") con aes(group = eps) y dibujar la destacada al final
# (ver la gráfica de líneas en scripts/03_graficas_espera_nacional.R).
colores_destacar <- function(niveles, destacar) {
  valores <- setNames(rep(colores_base[["gris_serie"]], length(niveles)), niveles)
  valores[destacar] <- paleta_categorica[seq_along(destacar)]
  valores
}

# Anotaciones -----------------------------------------------------------------

# Texto estándar de fuente para plot.caption.
fuente <- function(...) {
  paste0("Fuente: ", paste(..., sep = "; "), ".")
}

# Hitos de política que conviene marcar en las series de tiempo.
# Pandemia y presupuestos máximos van juntos: separados, sus etiquetas se solapan.
eventos_politica <- data.frame(
  fecha = as.Date(c("2020-03-01", "2024-04-01")),
  etiqueta = c("Pandemia y presupuestos máximos", "Intervención Nueva EPS")
)

# Capa con líneas verticales y etiquetas para los hitos, arriba del panel.
# Requiere un eje x de tipo Date; filtrar `eventos` si no aplican todos.
# Dejar espacio arriba: scale_y_continuous(expand = expansion(mult = c(0, 0.12))).
capa_eventos <- function(eventos = eventos_politica) {
  list(
    geom_vline(data = eventos, aes(xintercept = fecha),
               colour = colores_base[["texto_muted"]], linewidth = 0.3,
               linetype = "dashed"),
    geom_label(data = eventos, aes(x = fecha, y = Inf, label = etiqueta),
               inherit.aes = FALSE, hjust = -0.03, vjust = 1.3, size = 3,
               colour = colores_base[["texto_sec"]], fill = colores_base[["fondo"]],
               linewidth = 0, label.padding = unit(0.15, "lines"))
  )
}
