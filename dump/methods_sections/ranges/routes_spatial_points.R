library(dplyr)
library(sp)
library(raster)
library(sf)

route_locations <- read.csv("analysis_walkthroughs/ranges/route_locations.csv")

rl <- SpatialPointsDataFrame(route_locations[,c("longitude", "latitude")], route_locations)
rcrs <-crs("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")
crs(rl) <- rcrs

rl2 <- st_as_sf(route_locations, coords = c("longitude", "latitude"), crs = rcrs)
library(sf)

library(tigris)

states_sh <- tigris::states()

state_names <- unique(rl2$regionname)

state_names_upper <- toupper(state_names)

state_info <- data.frame(
  state_name = state_names,
  in_states = NA,
  nroutes_sf = NA,
  nroutes_bbs = NA,
  nroutes_match = NA
)

states_sh <- states_sh %>%
  mutate(NAME = toupper(NAME))

# demonstrates that the routes all land in the right states. takes about half an hour on my MBA.

for(i in 1:length(state_names)) {
#for(i in 1:10) {

  if(state_names_upper[i] %in% toupper(states_sh$NAME)) {

    this_state <- filter(states_sh, NAME == state_names_upper[i])

    stateroutes <- st_intersection(rl2, this_state)

    stateroutes2 <- filter(rl2, regionname == state_names[i])

    state_info$in_states[i] = T
    state_info$nroutes_sf[i] = nrow(stateroutes)
    state_info$nroutes_bbs[i] = nrow(stateroutes2)
    state_info$nroutes_match[i] = nrow(stateroutes) == nrow(stateroutes2)

  } else {

    state_info$in_states = F
  }

}
