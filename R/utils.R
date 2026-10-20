guardar_grafica <- function(grafica, nombre, ancho = 8, alto = 5, dpi = 300) {
  dir.create(here::here("figures"), showWarnings = FALSE)
  for (ext in c("svg", "png")) {
    ggplot2::ggsave(
      filename = here::here("figures", paste0(nombre, ".", ext)),
      plot = grafica, width = ancho, height = alto, dpi = dpi,
      bg = colores_base[["fondo"]]
    )
  }

  svg <- here::here("figures", paste0(nombre, ".svg"))
  contenido <- readLines(svg, warn = FALSE)
  contenido <- gsub("font-family: [^;\"]*\"[^\"]*\"", "font-family: system-ui, Segoe UI, Helvetica, Arial, sans-serif", contenido)
  writeLines(contenido, svg)

  invisible(grafica)
}
