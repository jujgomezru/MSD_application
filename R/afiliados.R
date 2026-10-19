# Afiliados activos por EPS: Base de Datos Única de Afiliados (BDUA) de la ADRES,
# regímenes contributivo (datos.gov.co, tq4m-hmg2) y subsidiado (d7a5-cnra).
# Es una foto del último corte publicado, no una serie histórica.

adres_bdua <- c(Contributivo = "tq4m-hmg2", Subsidiado = "d7a5-cnra")

# Devuelve ent_id (código de la entidad, el mismo de los reclamos de la
# Supersalud), ent_nombre, regimen y afiliados. La suma se hace en el servidor.
adres_afiliados <- function(archivo = here::here("data", "raw", "adres", "afiliados_eps.csv")) {
  if (!file.exists(archivo)) {
    dir.create(dirname(archivo), recursive = TRUE, showWarnings = FALSE)
    purrr::imap(adres_bdua, \(id, regimen) {
      httr2::request(sprintf("https://www.datos.gov.co/resource/%s.json", id)) |>
        httr2::req_url_query(
          `$select` = "ent_id, ent_nombre, sum(cantidad::number) as afiliados",
          `$group` = "ent_id, ent_nombre",
          `$limit` = 5000
        ) |>
        httr2::req_timeout(600) |>
        httr2::req_retry(max_tries = 3) |>
        httr2::req_perform() |>
        httr2::resp_body_json(simplifyVector = TRUE) |>
        dplyr::mutate(regimen = regimen, afiliados = as.numeric(afiliados))
    }) |>
      purrr::list_rbind() |>
      readr::write_csv(archivo)
  }
  readr::read_csv(archivo, show_col_types = FALSE)
}

# Agrupa los códigos de entidad por EPS: una EPS tiene un código por régimen y
# otro de movilidad ("-CM"), y los reclamos se radican con el código principal.
# Devuelve ent_id, eps_grupo (nombre de la ADRES sin "-CM") y afiliados_eps
# (afiliados de todos los códigos de la EPS).
adres_eps_grupos <- function(afiliados = adres_afiliados()) {
  afiliados |>
    dplyr::mutate(eps_grupo = stringr::str_squish(stringr::str_remove(ent_nombre, "\\s*-CM\\s*$"))) |>
    dplyr::mutate(afiliados_eps = sum(afiliados), .by = eps_grupo) |>
    dplyr::distinct(ent_id, eps_grupo, afiliados_eps)
}

# Tabla de reclamos por EPS con valores totales y por 10.000 afiliados.
# `reclamos` debe tener eps_codigo, eps (nombre corto de la Supersalud) y las
# columnas numéricas indicadas en `columnas`.
reclamos_por_afiliados <- function(reclamos, columnas, minimo_afiliados = 50000) {
  grupos <- adres_eps_grupos()
  reclamos |>
    # Algunos nombres traen comillas mal formadas (""CCF ORIENTE """"COMFAORIENTE"""").
    dplyr::mutate(eps = stringr::str_squish(gsub('"', "", eps, fixed = TRUE))) |>
    dplyr::inner_join(grupos, by = c("eps_codigo" = "ent_id")) |>
    dplyr::summarise(
      # Nombre corto: el que más usa la Supersalud para esa EPS.
      eps = names(sort(table(eps), decreasing = TRUE))[1],
      afiliados = dplyr::first(afiliados_eps),
      dplyr::across(dplyr::all_of(columnas), sum),
      .by = eps_grupo
    ) |>
    # Una misma EPS puede tener dos razones sociales en la ADRES (p. ej. Comfenalco
    # Valle en cada régimen): se unen por el nombre corto.
    dplyr::summarise(dplyr::across(c(afiliados, dplyr::all_of(columnas)), sum), .by = eps) |>
    dplyr::filter(afiliados >= minimo_afiliados) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(columnas), \(x) x / afiliados * 1e4, .names = "{.col}_10k"))
}
