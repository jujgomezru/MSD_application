# Descarga las bases de reclamos en salud de la Supersalud (2017 al segundo
# semestre de 2025, unos 5,9 GB) a data/raw/supersalud/. Si se interrumpe, al
# volver a ejecutarlo continúa con los archivos que falten.

source(here::here("R", "supersalud.R"))

supersalud_descargar()
