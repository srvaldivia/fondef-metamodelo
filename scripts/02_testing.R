
# 0. packages ------------------------------------------------------------
library(tidyverse)
library(sf)
library(janitor)
library(mapview)
# library(qgisprocess)


# 1. import data ---------------------------------------------------------
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

# read_rds("datos_originales/r07_dbscan.rds") |> 
#   pluck("points") |> 
#   tibble() |>
#   st_as_sf() |> 
#   st_write(
#   viviendas,
#   dsn = "datos_originales/SIG_consolidados.gdb",
#   layer = "puntos_dbscan_r07",
#   delete_layer = TRUE
# )


viviendas <- 
  read_rds("datos_originales/r07_dbscan.rds") |> 
  pluck("points") |> 
  tibble() |>
  st_as_sf() |> 
  filter(cluster != 0) |>
  mutate(
    id_cluster =
      str_c("r07_", n_comuna, "_dbscan-", str_pad(cluster, 3, pad = "0")),
      # str_c("r07_", n_comuna, "_dbscan-", cluster),
  ) |> 
  tibble() |> 
  rename(geometry = SHAPE) |> 
  clean_names() |> 
  select(-fid_via) |> 
  st_as_sf(
)


redes <-
  st_read(
    dsn = "datos_originales/apc2023_r07.gdb",
    layer = "Eje_Vial",
    as_tibble = TRUE
  ) |> 
  st_transform(32719) |> 
  st_cast("MULTILINESTRING") |> 
  st_cast("LINESTRING") |> 
  clean_names() |> 
  select(-shape_length) |> 
  mutate(
    geom_vial = SHAPE,
    fid_via = row_number()
)

cl <-
  st_read(
    dsn = "datos_originales/hulls_consolidado_comuna.gpkg",
    layer = "clusters",
    as_tibble = TRUE
  ) |> 
  filter(region == "r07" & modelo == "dbscan") |> 
  select(region:id_cluster, n_puntos
)



# 2. fix errors ----------------------------------------------------------
## 2.1 eliminar clusters (vivienda) con menos de 20 puntos ----
viviendas <- viviendas |> 
  group_by(id_cluster) |>
  mutate(n_puntos = n()) |>
  ungroup() |> 
  filter(n_puntos >= 20
)

## 2.1 reclasificar clase_comuna ----
redes <- redes |>
  mutate(
    clase_comuna = str_to_title(clase_comuna),
    clase_comuna =
      case_when(
        clase_comuna == "Bajonivel"  ~ "bajo_nivel",
        clase_comuna == "Sobrenivel" ~ "Sobre_nivel",
        clase_comuna == "N/A"        ~ "Urbano",
        .default = clase_comuna
      ),
    clase_comuna =
      fct_relevel(
        .f = clase_comuna,
        c("Carretera", "Principal", "Secundario", "Urbano",
          "Camino","Privado", "Peatonal", "Huella",
          "Sendero","Puente", "Sobre_nivel", "bajo_nivel")
      )
)



# 3. atributar FID vial + cercano a viviendas ----------------------------
# viviendas <- viviendas |>
#   st_join(
#     y = redes |> select(fid_via, geom_vial),
#     join = st_nearest_feature
#   ) |>
#   rowwise() |>
#   mutate(dist_near_via = st_distance(geom_vial, geometry)[,1]) |>
#   ungroup()

idx_nearest <- st_nearest_feature(viviendas, redes)
viviendas <- viviendas |>
  mutate(
    fid_via_nearest = redes$fid_via[idx_nearest],
    dist_near_via   = st_distance(
      viviendas,
      redes[idx_nearest, ],
      by_element = TRUE
    )
)




# 4. Crear nuevas geometrias cluster -------------------------------------
## 4.1 resumir por id_cluster y fid via ----
cluster_lineal <- viviendas |> 
  st_drop_geometry() |> 
  group_by(id_cluster, fid_via_nearest) |> 
  summarise(
    puntos = n(),
    mean_dist = as.numeric(mean(dist_near_via, na.rm = TRUE))
  ) |>
  ungroup() |> 
  left_join(
    y = redes |> st_drop_geometry() |> select(fid_via, geom_vial),
    by = c("fid_via_nearest" = "fid_via")
  ) |> 
  st_as_sf(
)

## 4.2 crear buffers rectos ----
buff_flat_raw <-
  st_buffer(
    cluster_lineal,
    dist = 50,
    endCapStyle = "FLAT",
)

buff_flat_geom <- buff_flat_raw |> 
  group_by(id_cluster) |> 
  summarise(
    n = n(),
    geom = st_union(geom_vial)
  ) |> 
  ungroup() |> 
  mutate(id_cluster = as_factor(id_cluster))

mapview(
  buff_flat_geom,
  zcol = "id_cluster",
  col.region = RColorBrewer::brewer.pal(12, "Set3"),
  legend = FALSE
)


# 3. GIS OVERLAY ---------------------------------------------------------
## 3.1 INTERSECT: ejes viales y clusters ----
vial_inter_cl <-
  st_intersection(x = redes, y = cl) |> 
  # select(cut:id_cluster) |>
  st_cast("MULTILINESTRING") |> 
  st_cast("LINESTRING") |> 
  select(comuna:n_puntos, cut:clase_urbana) |> 
  mutate(via_length = as.numeric(st_length(SHAPE)) / 1000
)


## 3.2 SUMMARISE interseccion ejes y cluster ----
summary_vial_inter <- vial_inter_cl |> 
  group_by(modelo, id_cluster, clase_comuna) |> 
  summarise(
    total_length_km = sum(via_length, na.rm = TRUE)
  ) |> 
  ungroup(
)


## 3.3 PIVOT WIDER summarise ----
longitud_vialidad_cl <- summary_vial_inter |> 
  group_by(modelo, id_cluster) |> 
  mutate(geometry_cl = st_union(SHAPE)) |> 
  ungroup() |> 
  st_set_geometry("geometry_cl") |>
  select(-SHAPE) |> 
  pivot_wider(
    id_cols     = c(modelo, id_cluster, geometry_cl),
    names_from  = clase_comuna,
    values_from = total_length_km,
    names_sort  = TRUE,
    values_fill = 0
  ) |> 
  clean_names() |> 
  rowwise() |> 
  mutate(
    total_long_km = sum(c_across(carretera:bajo_nivel), na.rm = TRUE),
    .after = geometry_cl
    ) |> 
  ungroup() |> 
  st_cast("MULTILINESTRING"
)



# 4. testing new geometries for cluster polygons -------------------------
buff_square <- st_buffer(x = longitud_vialidad_cl, dist = 50, endCapStyle = "SQUARE") |> 
  st_cast("MULTIPOLYGON")

buff_flat <- st_buffer(x = longitud_vialidad_cl, dist = 50, endCapStyle = "FLAT") |> 
  st_cast("MULTIPOLYGON")


# 5. EXPORT --------------------------------------------------------------
st_write(
  longitud_vialidad_cl,
  dsn = "datos_resultados/GIS_resultados.gdb",
  layer = "predictores_longitud_vialidad",
  delete_layer = TRUE
)

st_write(
  buff_square,
  dsn = "datos_resultados/GIS_resultados.gdb",
  layer = "buff_square",
  delete_layer = TRUE
)

st_write(
  buff_flat,
  dsn = "datos_resultados/GIS_resultados.gdb",
  layer = "buff_flat",
  delete_layer = TRUE
)
