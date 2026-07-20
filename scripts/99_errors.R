# viviendas <-
#   st_read(
#     dsn = "datos_originales/pts_consolidado_comuna.gpkg",
#     layer = "puntos",
#     as_tibble = TRUE,
#     query = "select * from puntos where region = 'r06' AND modelo = 'dbscan'"
#     # query = "select * from puntos where region = 'r07'"
#   ) |> 
#   st_transform(32719
# )