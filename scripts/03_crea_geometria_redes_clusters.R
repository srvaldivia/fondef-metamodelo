# 0. packages ------------------------------------------------------------
library(tidyverse)
library(sf)
library(janitor)
library(mapview)
library(leaflet.extras2)

source("scripts/00_fns.R")


# 1. import data ---------------------------------------------------------

puntos <-
  st_read(
    "datos_originales/pts_consolidado_comuna.gpkg",
    # layer = "puntos",
    as_tibble = TRUE,
    query = "select * from puntos where region = 'r07' AND modelo = 'dbscan'"
    # query = "select * from puntos where region = 'r07'"
)


viviendas <-
  st_read(
    dsn = "datos_originales/dataset_base.gdb",
    layer = "viviendas",
    as_tibble = TRUE
  ) |>
  rename(geometry = SHAPE
)

redes <-
  st_read(
    dsn = "datos_originales/dataset_base.gdb",
    layer = "redes",
    as_tibble = TRUE
  ) |>
  rename(geometry = SHAPE) |>
  mutate(geom_vial = geometry
)

hulls <-
  st_read(
    dsn = "datos_originales/dataset_base.gdb",
    layer = "hulls_clusters",
    as_tibble = TRUE
  ) |>
  rename(geometry = SHAPE
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
    dist = 100,
    endCapStyle = "FLAT",
)

## 4.2 dissolve por id_cluster ----
buff_flat_geom <- buff_flat_raw |>
  group_by(id_cluster) |>
  summarise(
    n = n(),
    geom = st_union(geom_vial)
  ) |>
  ungroup() |>
  mutate(id_cluster = as_factor(id_cluster)) |> 
  st_cast("MULTIPOLYGON"
)

## 4.3 eliminar hoyos internos en polígonos ----
cluster_vial_polygon <- fill_polygons(buff_flat_geom, threshold = 0.99)
redes_cluster_vial <- st_filter(redes, cluster_vial_polygon, .predicate = st_intersects)


# mapview(
#   cluster_vial_polygon,
#   zcol = "id_cluster",
#   col.region = RColorBrewer::brewer.pal(12, "Set3"),
#   legend = FALSE
# )




# 5. exportar ------------------------------------------------------------
# st_write(
#   obj = cluster_vial_polygon,
#   dsn = "datos_resultados/GIS_temp.gdb",
#   layer = "polygonos_cluster_v1",
#   delete_layer = TRUE
# )

# st_write(
#   obj = redes_cluster_vial,
#   dsn = "datos_resultados/GIS_temp.gdb",
#   layer = "redes_cluster",
#   delete_layer = TRUE
# )
