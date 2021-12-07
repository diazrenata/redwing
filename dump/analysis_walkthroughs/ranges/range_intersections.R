library(dplyr)
library(sp)
library(raster)
library(sf)

matched_names <- read.csv(here::here("analysis_walkthroughs", "ranges", "matched_names_done.csv"))


route_locations <- read.csv("analysis_walkthroughs/ranges/route_locations.csv")

rl <- SpatialPointsDataFrame(route_locations[,c("longitude", "latitude")], route_locations)
rcrs <-crs("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")
crs(rl) <- rcrs

allsp <- st_read("analysis_walkthroughs/ranges/species_in_bbs/species_in_bbs/species_in_bbs.shp")

crs(allsp)
crs(rl)

rl_sf <- st_as_sf(rl)

rl_sf_transformed <- sf::st_transform(rl_sf, crs = crs(allsp))

crs(rl_sf_transformed)
crs(allsp)

## Restrict to EXTANT, RESIDENT or BREEDING
#
# allsp_filtered <- allsp %>%
#   filter(presence == 1, seasonal %in% c(1, 2))

get_route_intersections <- function(range_feature, routes_sf) {

  sf_use_s2(FALSE)

  if(!(sf::st_is_valid(range_feature))) {
    range_feature <- sf::st_make_valid(range_feature)
  }

  route_range_intersect <- sf::st_intersection(range_feature, routes_sf)

  return(as.data.frame(route_range_intersect))

}

pull_range_feature <- function(range_feature_index, range_features) {

  return(range_features[i, ])

}


#get_route_intersections(allsp_filtered[37,], rl_sf_transformed)

route_intersections <- list()

for(i in 1:5) {

  route_intersections[[i]] <- get_route_intersections(allsp_filtered[i, ], rl_sf_transformed)

}

ri_df <- bind_rows(route_intersections)
