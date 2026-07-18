
# 0. packages ------------------------------------------------------------
library(tidyverse)
library(sf)
library(janitor)
# library(qgisprocess)


# 1. import data ---------------------------------------------------------
# st_layers(dsn = "datos_originales/apc2023_r07.gdb")
# st_layers(dsn = "datos_originales/hulls_consolidado_comuna.gpkg")
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
  select(-shape_length
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


# 2. ordenar -------------------------------------------------------------
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


## 3.2 SUMMARISE intersect ----
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
