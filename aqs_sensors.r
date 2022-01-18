library(tidyverse)
library(sf)
library(leaflet)

d_aqs <- s3::s3_get("s3://geomarker/st_pm_hex/h3data_aqs.qs") %>%
  qs::qread()


d <- d_aqs %>%
  group_by(h3) %>%
  add_count(name = "count") %>%
  as.data.frame() %>%
  distinct(h3, .keep_all = T)

# install.packages("remotes")
remotes::install_github("crazycapivara/h3-r")
library(h3)

d$h3 <- as.character(d$h3)

d_aqs_sf <- d %>%
  mutate(h3_to_geo_boundary_sf(h3)) %>%
  st_as_sf()

mapview::mapview(d_aqs_sf, zcol = 'count')

saveRDS(d_aqs_sf, 'aqs_censors.rds')



#testing leaflet
d <- readRDS("d_map_long.rds") %>%
  st_as_sf() %>%
  filter(metric == 'rmse')
d_aqs <- read_rds('aqs_censors.rds') %>%
  sf::st_as_sf()

leaflet(d) %>%
  setView(-93.65, 38.0285, zoom = 4.5) %>%
  addPolygons(fillColor = 'value') %>%
  addPolygons(data = d_aqs, fillColor = count, opacity = 1, fillOpacity = 1)


