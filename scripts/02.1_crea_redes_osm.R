# 0. packages ------------------------------------------------------------
library(tidyverse)
library(sf)
library(janitor)
library(mapview)
library(osmdata)
library(sfnetworks)
library(tidygraph)



# 1. import data ---------------------------------------------------------
hulls <-
  st_read(
    dsn = "datos_originales/dataset_base.gdb",
    layer = "hulls_clusters",
    as_tibble = TRUE
  ) |>
  rename(geometry = SHAPE
)


# 2. definir bbox para area estudio --------------------------------------
aoi <-
  hulls |> 
  st_buffer(dist = 5000) |>
  st_transform(4326) |> 
  st_bbox(
)


# 3. descarga desde OSM --------------------------------------------------
osmdata::set_overpass_url("https://overpass-api.de/api/interpreter")
redes_osm <-
  opq(bbox = aoi, timeout = 300) |>
  add_osm_feature(key = "highway") |>
  osmdata_sf()

 redes_osm <- redes_osm |> 
  pluck("osm_lines") |> 
  tibble() |> 
  st_as_sf() |> 
  st_make_valid() |> 
  st_cast("LINESTRING")


# 4. planarize -----------------------------------------------------------
net <- as_sfnetwork(redes_osm)
simple <- net |> 
  activate("edges") |> 
  filter(!edge_is_multiple()) |> 
  filter(!edge_is_loop()
)
osm_planarize <- convert(simple, to_spatial_subdivision)
redes <- st_as_sf(osm_planarize)

# 5. export --------------------------------------------------------------
redes |>
  clean_names() |> 
  select(osm_id:geometry) |>
  mutate(osm_id = row_number()) |> 
  st_write(dsn = "datos_originales/dataset_base.gdb", layer = "red_osm", delete_layer = TRUE)