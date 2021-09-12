library(dplyr)
library(sp)
library(raster)

route_locations <- read.csv("analysis_walkthroughs/ranges/route_locations.csv")

rl <- SpatialPointsDataFrame(route_locations[,c("longitude", "latitude")], route_locations)
rcrs <-crs("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")
crs(rl) <- rcrs
library(sf)

allsp <- st_read("analysis_walkthroughs/ranges/species_in_bbs/species_in_bbs/species_in_bbs.shp")

rl_spcrs <- spTransform(rl, crs(allsp))
crs(rl_spcrs)
