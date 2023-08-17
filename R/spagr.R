library(stars)
library(sf)
library(tidyverse)
library(magrittr)
library(progress)

chirps_rast <- read_stars("chirps.tif")
vnm_shp <- read_rds("gadm41_VNM_1_pk.rds") %>%
  terra::unwrap() %>%
  st_as_sf()

spagr <- function(rast, shp, method = 1, show_progress = TRUE,...) {
  if (!inherits(rast, "stars")) {
    stop("`rast` has to be a `stars` object")
  }
  if (!inherits(shp, "sf")) {
    stop("`shp` has to be an `sf` object")
  }
  if (!inherits(id_col, "character") || !(id_col %in% colnames(shp))) {
    stop("`id_col` has to be a column name in `shp`")
  }
  if (!inherits(method, "numeric") || !(method %in% c(1, 2, 3))) {
    stop("`method` has to be a number between 1-3")
    return
  }

  spagr_(
    rast = rast,
    shp = shp,
    method = method,
    show_progress = show_progress
  )
}

spagr_ <- function(rast, shp, method, show_progress) {
  methods <- c(method_1, method_2, method_3)
  method <- methods[[method]]

  rast <- st_crop(rast, shp)

  method(rast = rast, shp = shp, show_progress = show_progress)
}

method_1 <- function(rast, shp, show_progress) {
  res_df <- tibble(id = character(), agg_val = double())

  if(show_progress)
    pb <- progress_bar$new(format = "[:bar] :current/:total (:percent)", total = nrow(shp))

  for (i in seq(1, nrow(shp))){
    poly <- shp[i, ]
    poly_centroid <- st_centroid(poly) %>% st_geometry()

    poly_rast <- st_crop(
      rast,
      st_buffer(
        poly,
        units::set_units(st_res(rast)[["x"]], "degree")
      )
    ) %>% st_as_sf() %>% st_centroid()%>%
      rename(val = colnames(.)[1])

    print("checkpoint 1")

    poly_rast %<>% mutate(
      id = poly$GID_2,
      poly_centroid = poly_centroid,
      dist = st_distance(poly_centroid, geometry, by_element = TRUE) %>% units::drop_units()
    )

    print("checkpoint 2")

    res_df %<>% bind_rows(
      poly_rast %>%
        group_by(id) %>%
        summarise(agg_val = sum(val / dist) / sum(1 / dist)) %>%
        as_tibble() %>%
        select(-geometry)
    )

    if(show_progress)
      pb$tick()
  }

  res_df
}

method_2 <- function(rast, shp) {

}

method_3 <- function(rast, shp) {

}

spagr(rast = chirps_rast, shp = vnm_shp, method = 1)

ggplot()+ geom_sf(data = shp) + geom_sf(
  data = shp[i, ], color = "red"
)
