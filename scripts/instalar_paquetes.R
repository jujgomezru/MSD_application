if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")

options(repos = c(CRAN = "https://packagemanager.posit.co/cran/latest"))

renv::init(bare = TRUE, restart = FALSE)

renv::install(c(
  "tidyverse",
  "here",
  "scales",
  "janitor",
  "readxl",
  "httr2",
  "rvest",
  "pdftools",
  "sf",
  "svglite",
  "knitr",
  "rmarkdown"
))

renv::snapshot(type = "all")
