library(stars)
library(sf)
library(tidyverse)
library(magrittr)
library(progress)

chirps_rast <- read_stars("chirps.tif")
vnm_shp <- read_sf("gadm41_VNM_2.shp")

spagr <- function(rast, shp, method = 1, ...) {
  if (class(rast)[1] != "stars") {
    print("`rast` has to be a `stars` object")
    return
  }
  if (class(shp)[1] != "sf") {
    print("`shp` has to be an `sf` object")
    return
  }
  if (class(method) != "numeric" && class(method) != "function") {
    print("`method` has to be a number between 1-3 or a defined function")
    return
  }
  if (!(method %in% c(1, 2, 3))) {
    print("`method` has to be a number between 1-3")
    return
  }

  spagr_(rast = rast, shp = shp, method = method)
}

spagr_ <- function(rast, shp, method) {
  method <- methods[method]
}
