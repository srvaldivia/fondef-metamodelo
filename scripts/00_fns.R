# RENOMBRA SFC ---------------------------------------------------------
safe_geom <- function(df) {
  es_sf <- inherits(df, "sf")
  
  if (es_sf) {
    nombre_geom <- attr(df, "sf_column")
    if (!is.null(nombre_geom) && nombre_geom != "geometry") {
      df <- df |>
        dplyr::rename(geometry = dplyr::all_of(nombre_geom)) |>
        sf::st_set_geometry("geometry")
      message("Columna sfc renombrada de '", nombre_geom, "' a 'geometry'.")
    }
  }
}



fill_polygons <- function(x, threshold = 0.1, remove_all = FALSE) {

  if (!inherits(x, c("sf", "sfc"))) {
    cli::cli_abort("{.arg x} debe ser un objeto {.cls sf} o {.cls sfc}.")
  }
  if (threshold < 0 || threshold > 100) {
    cli::cli_abort("{.arg threshold} debe estar entre 0 y 100 (porcentaje).")
  }

  is_sf <- inherits(x, "sf")
  geom  <- if (is_sf) sf::st_geometry(x) else x
  crs   <- sf::st_crs(geom)

  # Seleccionar función de área según CRS
  if (!sf::st_is_longlat(geom)) {
    # Proyectado: Shoelace directo sobre coordenadas (sin overhead sf)
    calc_area <- function(coords) {
      n <- nrow(coords) - 1L
      x <- coords[seq_len(n), 1]
      y <- coords[seq_len(n), 2]
      abs(sum(x * c(y[-1], y[1]) - c(x[-1], x[1]) * y)) / 2
    }
  } else {
    # Geográfico: necesita st_area para cálculo geodésico
    calc_area <- function(coords) {
      as.numeric(sf::st_area(sf::st_sfc(sf::st_polygon(list(coords)), crs = crs)))
    }
  }

  fill_poly <- function(poly_coords, area_ref) {
    exterior <- poly_coords[[1]]
    if (length(poly_coords) <= 1L || remove_all) return(list(exterior))

    holes <- poly_coords[-1]
    keep  <- vapply(holes, function(h) {
      (calc_area(h) / area_ref * 100) >= threshold
    }, logical(1))

    c(list(exterior), holes[keep])
  }

  new_geom <- lapply(geom, function(g) {
    if (sf::st_is(g, "POLYGON")) {
      area_ref <- calc_area(unclass(g)[[1]])
      sf::st_polygon(fill_poly(unclass(g), area_ref))

    } else if (sf::st_is(g, "MULTIPOLYGON")) {
      parts    <- unclass(g)
      area_ref <- sum(vapply(parts, function(p) calc_area(p[[1]]), numeric(1)))
      sf::st_multipolygon(lapply(parts, fill_poly, area_ref = area_ref))

    } else {
      g
    }
  })

  new_sfc <- sf::st_sfc(new_geom, crs = crs)

  if (is_sf) {
    sf::st_geometry(x) <- new_sfc
    return(x)
  }
  new_sfc
}
