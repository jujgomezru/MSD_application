# Carga paquetes y funciones del proyecto. Cada .qmd y script empieza con:
# source(here::here("R", "setup.R"))

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(scales)
})

source(here("R", "tema.R"))
source(here("R", "utils.R"))

theme_set(theme_informe())
