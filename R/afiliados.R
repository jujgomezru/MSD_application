adres_bdua <- c(Contributivo = "tq4m-hmg2", Subsidiado = "d7a5-cnra")

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

adres_eps_grupos <- function(afiliados = adres_afiliados()) {
  afiliados |>
    dplyr::mutate(eps_grupo = stringr::str_squish(stringr::str_remove(ent_nombre, "\\s*-CM\\s*$"))) |>
    dplyr::mutate(afiliados_eps = sum(afiliados), .by = eps_grupo) |>
    dplyr::distinct(ent_id, eps_grupo, afiliados_eps)
}

reclamos_por_afiliados <- function(reclamos, columnas, minimo_afiliados = 50000) {
  grupos <- adres_eps_grupos()
  reclamos |>
    dplyr::mutate(eps = stringr::str_squish(gsub('"', "", eps, fixed = TRUE))) |>
    dplyr::inner_join(grupos, by = c("eps_codigo" = "ent_id")) |>
    dplyr::summarise(
      eps = names(sort(table(eps), decreasing = TRUE))[1],
      afiliados = dplyr::first(afiliados_eps),
      dplyr::across(dplyr::all_of(columnas), sum),
      .by = eps_grupo
    ) |>
    dplyr::summarise(dplyr::across(c(afiliados, dplyr::all_of(columnas)), sum), .by = eps) |>
    dplyr::filter(afiliados >= minimo_afiliados) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(columnas), \(x) x / afiliados * 1e4, .names = "{.col}_10k"))
}
