
# 0. packages ------------------------------------------------------------
library(httr2)
library(cli)
library(stringr)
library(sf)


# 1. set variables -------------------------------------------------------
url <- "https://www.ine.gob.cl/docs/default-source/geodatos-abiertos/cartografia/actualización-cartográfica-continua/actualización-precensal-2023/gdb/apc2023_r07-gdb.zip?sfvrsn=24cef463_4"
file_downloaded <- "datos_originales/apc2023_r07-gdb.zip"
folder_output <- "datos_originales"
gdb_file <- basename(file_downloaded) |> stringr::str_replace("-gdb.zip", ".gdb")


# 2. download and unzip --------------------------------------------------
if (!file.exists(file_downloaded)) {
  request(url) |> req_perform(path = file_downloaded)
  unzip(file_downloaded, exdir = folder_output)
  cli_alert_success("El archivo {.file {gdb_file}} se descargó exitosamente.")
} else {
  cli_alert_info("El archivo {.file {gdb_file}} ya existe.")
}


# 3. checking ------------------------------------------------------------
st_layers(dsn = file.path(folder_output, gdb_file))
