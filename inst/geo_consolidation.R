
library(sf)
library(luaRlp)

centroid <-  st_centroid(luaRlp::geo_standards$geom)

gs <- cbind(geo_standards, centroid)


library(dplyr)

gs <- gs %>% rename(
  centroid = geometry,
  umriss = geom
)

geo_standards <- gs


save(geo_standards, file="data/geo_standards.rda")

