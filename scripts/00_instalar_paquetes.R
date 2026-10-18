# Instala los paquetes del proyecto y fija sus versiones con renv.
# Ejecutar una sola vez, desde la raíz del proyecto. En otro equipo basta con
# renv::restore().
#
# En Ubuntu, sf y pdftools necesitan librerías del sistema:
#   sudo apt-get install -y libgdal-dev libgeos-dev libproj-dev libudunits2-dev libpoppler-cpp-dev

if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")

# Posit Package Manager ofrece binarios precompilados para Windows y Linux;
# sin él, en Linux todos los paquetes se compilan desde el código fuente.
options(repos = c(CRAN = "https://packagemanager.posit.co/cran/latest"))

renv::init(bare = TRUE, restart = FALSE)

renv::install(c(
  "tidyverse",  # ggplot2, dplyr, readr, tidyr, purrr, stringr...
  "here",       # rutas relativas a la raíz del proyecto
  "scales",     # formatos de ejes (porcentajes, miles)
  "janitor",    # limpiar nombres de columnas
  "readxl",     # Excel de SISPRO, ADRES y CAC
  "httr2",      # descargas (reportes del Observatorio de Calidad)
  "rvest",      # leer formularios HTML
  "pdftools",   # texto de informes en PDF (Defensoría, CAC)
  "sf",         # mapas por departamento (MGN del DANE)
  "svglite",    # exportar SVG
  "knitr",
  "rmarkdown"
))

renv::snapshot(type = "all")
