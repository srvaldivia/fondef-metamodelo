# 0. packages ------------------------------------------------------------
library(tidyverse)
library(sf)
library(janitor)
library(mapview)
library(qgisprocess)


# 1. import data ---------------------------------------------------------
viviendas <-
  st_read(
    dsn = "datos_originales/dataset_base.gdb",
    layer = "viviendas",
    as_tibble = TRUE
  ) |>
  rename(geometry = SHAPE
)

hulls <-
  st_read(
    dsn = "datos_originales/dataset_base.gdb",
    layer = "hulls_clusters",
    as_tibble = TRUE
  ) |>
  rename(geometry = SHAPE) |> 
  mutate(geometry_cl = geometry)

redes_osm <-
  st_read(
    dsn = "datos_originales/dataset_base.gdb",
    layer = "red_osm",
    as_tibble = TRUE
  ) |>
  st_transform(32719) |> 
  rename(geometry = SHAPE) |> 
  # mutate(geom_vial = geometry) |>
  st_cast("LINESTRING") |> 
  mutate(
    clase_via = 
      case_when(
        surface %in% c("asphalt", "concrete", "concrete:plates", "paved", "chipseal") ~ "Pavimento",
        surface %in% c("paving_stones", "sett", "cobblestone", "unhewn_cobblestone", "pebblestone") ~ "Adoquines/empedrado",
        surface %in% c("compacted", "fine_gravel", "gravel", "Gavilla") ~ "Ripio",
        surface %in% c("ground", "dirt", "earth", "dirt/sand", "sand", "grass", "unpaved", "alternating", "ungr") ~ "Huella",
        .default = NA_character_
      ),
    jerarquia_via =
      case_when(
        highway %in% c("motorway", "motorway_link") ~ "Autopista",
        highway %in% c("trunk", "trunk_link") ~ "Troncal",
        highway %in% c("primary", "primary_link") ~ "Primaria",
        highway %in% c("secondary", "secondary_link") ~ "Secundaria",
        highway %in% c("tertiary", "tertiary_link") ~ "Terciaria",
        highway %in% c("unclassified", "residential", "living_street", "service") ~ "Local",
        highway == "track" ~ "Rural",
        highway %in% c("cycleway", "pedestrian", "footway", "path", "bridleway", "steps") ~ "No motorizada",
        highway %in% c("construction", "proposed", "planned") ~ "Temporal",
        .default = "Otro"
      ),
    jerarquia_via =
      fct_relevel(jerarquia_via, 
                  c("Autopista", "Troncal", "Primaria",
                  "Secundaria", "Terciaria", "Local",
                  "Rural", "No motorizada", "Temporal", "Otro"))
      
)


# 2. fix errors ----------------------------------------------------------
## 2.1 eliminar clusters (vivienda) con menos de 20 puntos ----
viviendas <- viviendas |> 
  group_by(id_cluster) |>
  mutate(n_puntos = n()) |>
  ungroup() |> 
  filter(n_puntos >= 20
)


# 3. CLIP: redes con hull ------------------------------------------------
redes_clip <- 
  qgis_run_algorithm(
    algorithm = "native:clip",
    INPUT     = redes_osm,
    OVERLAY   = hulls
  ) |>
  st_as_sf() |> 
  mutate(
    jerarquia_via =
      fct_relevel(jerarquia_via, 
                  c("Autopista", "Troncal", "Primaria",
                  "Secundaria", "Terciaria", "Local",
                  "Rural", "No motorizada", "Temporal", "Otro"))
)


# 4. SPATIAL JOIN: redes con hull ----------------------------------------
sj_redes_hull <- 
  st_join(
    x = redes_clip,
    y = hulls |> select(modelo, id_cluster, geometry_cl),
    left = FALSE
  ) |> 
  mutate(length_via_km = as.numeric(st_length(geom))/ 1000
)



# 5. SUMMARISE spatial join ----------------------------------------------
summary_vial_inter <- sj_redes_hull |> 
  group_by(modelo, id_cluster, jerarquia_via, geometry_cl) |> 
  summarise(
    total_length_km = sum(length_via_km, na.rm = TRUE)
  ) |> 
  ungroup(
)


## 3.3 PIVOT WIDER summarise ----
longitud_vialidad_cl <- summary_vial_inter |> 
  # group_by(modelo, id_cluster) |> 
  # mutate(geometry_cl = st_union(SHAPE)) |> 
  # ungroup() |> 
  # st_set_geometry("geometry_cl") |>
  # select(-SHAPE) |> 
  pivot_wider(
    id_cols     = c(modelo, id_cluster, geometry_cl),
    names_from  = jerarquia_via,
    values_from = total_length_km,
    names_sort  = TRUE,
    values_fill = 0
  ) |> 
  clean_names() |> 
  rowwise() |> 
  mutate(
    total_long_km = sum(c_across(autopista:temporal), na.rm = TRUE),
    .after = geometry_cl
    ) |> 
  ungroup(
)

mapview(longitud_vialidad_cl, zcol = "total_long_km")


