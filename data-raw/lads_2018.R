# GET LAD SPATIAL DATA
# NOTE: This is to the full resolution file as the lower resolution data omits schools that are on the boundaries cut out when generalised this is therefore a large download

geojson_url <-"https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/LAD_Dec_2018_Boundaries_UK_BFE_2022/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json"
#

# create a temporary directory
tmp_dir <- tempdir()

## create path for geojson data
file_path_lads_2018 <- file.path(tmp_dir, "location_data_2018.geojson")

# download from the URL to the filepath specified
if(!file.exists(file_path_lads_2018)){
  utils::download.file(geojson_url, mode = "wb", method = "libcurl", destfile = file_path_lads_2018)
}


lads_2018 <- sf::read_sf(file_path_lads_2018)

usethis::use_data(lads_2018, compress = "xz", internal = TRUE)
