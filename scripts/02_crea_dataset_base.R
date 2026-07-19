
# 0. packages ------------------------------------------------------------
library(tidyverse)
library(sf)
library(janitor)
library(mapview)
# library(qgisprocess)


# 1. import data ---------------------------------------------------------
read_rds("datos_originales/r07_dbscan.rds") |> 
  pluck("points") |> 
  tibble() |>
  st_as_sf() |> 
  filter(cluster != 0) |>
  mutate(
    id_cluster =
      str_c("r07_", n_comuna, "_dbscan-", str_pad(cluster, 3, pad = "0")),
  ) |> 
  tibble() |> 
  rename(geometry = SHAPE) |> 
  clean_names() |> 
  select(-fid_via) |> 
  st_as_sf() |> 
  st_write(
    dsn = "datos_originales/dataset_base.gdb",
    layer = "viviendas",
    delete_layer = TRUE
)


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
  ) |> 
  rename(geometry = SHAPE) |> 
  st_write(
    dsn = "datos_originales/dataset_base.gdb",
    layer = "redes",
    delete_layer = TRUE
)


st_read(
    dsn = "datos_originales/hulls_consolidado_comuna.gpkg",
    layer = "clusters",
    as_tibble = TRUE
  ) |> 
  filter(region == "r07" & modelo == "dbscan") |> 
  select(region:id_cluster, n_puntos) |> 
  st_write(
    dsn = "datos_originales/dataset_base.gdb",
    layer = "hulls_clusters",
    delete_layer = TRUE
)
