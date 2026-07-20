  eliminar_hoyos <- function(geom, threshold_pct) {
    area_total <- as.numeric(sum(st_area(st_cast(geom, "POLYGON"))))
    threshold <- area_total * threshold_pct / 100
    polys <- st_cast(geom, "POLYGON")

    polys_limpios <- lapply(seq_along(polys), function(i) {
      rings <- polys[[i]][]
      exterior <- rings[[1]]
      if (length(rings) > 1) {
        hoyos <- rings[-1]
        areas <- sapply(hoyos, function(h) {
          abs(as.numeric(st_area(st_polygon(list(h)))))
        })
        hoyos <- hoyos[areas > threshold]
        st_polygon(c(list(exterior), hoyos))
      } else {
        st_polygon(list(exterior))
      }
    })

    st_sfc(polys_limpios, crs = st_crs(geom)) |> st_union()
  }