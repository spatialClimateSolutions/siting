library(flexdashboard)
library(tidyverse)
library(sf)
library(terra)
library(stringr)
library(knitr)
library(RColorBrewer)
library(mapview)

aoi <- read_sf("./data/masks/us_outline/with_states.shp")

########Variables############
path <- "./data"

display <- function(file, legend=TRUE) {
  path_to_file <- file.path(path, file)
  x <- if(grepl(pattern = ".tif", x = file)) {rast(path_to_file)} else {vect(path_to_file)}
  us <- terra::project(vect(aoi), x)
  terra::plot(x, axes = FALSE, alpha = 0, legend = legend)
  terra::plot(us, axes=FALSE, alpha = 0.7, border = "grey", add = TRUE)
  terra::plot(x, axes=FALSE, add = TRUE, legend = legend)
}


display_mapview_quality <- function(file, layer_name = "value", legend = TRUE) {
  path_to_file <- file.path(path, file)
  x <- raster::raster(path_to_file)
  mapview::mapviewOptions(raster.size=Inf)
  mapview::mapView(x, trim = FALSE, layer.name = layer_name, alpha.regions = 0.7, na.color = "transparent", legend = legend, maxpixels = 2150000)
}



