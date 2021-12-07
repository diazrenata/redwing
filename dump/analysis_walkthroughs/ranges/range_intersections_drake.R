library(dplyr)
library(sp)
library(raster)
library(sf)
library(drake)

allsp <- st_read("analysis_walkthroughs/ranges/species_in_bbs/species_in_bbs/species_in_bbs.shp")
# matched_names <- read.csv(here::here("analysis_walkthroughs", "ranges", "matched_names_done.csv"))

allsp <- allsp[1:10, ]

route_locations <- read.csv("analysis_walkthroughs/ranges/route_locations.csv")

rl <- SpatialPointsDataFrame(route_locations[,c("longitude", "latitude")], route_locations)
rcrs <-crs("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")
crs(rl) <- rcrs

rl_sf <- st_as_sf(rl)

rl_sf_transformed <- sf::st_transform(rl_sf, crs = crs(allsp))


get_route_intersections <- function(range_feature, routes_sf) {

  sf_use_s2(FALSE)

  if(!(sf::st_is_valid(range_feature))) {
    range_feature <- sf::st_make_valid(range_feature)
  }

  route_range_intersect <- sf::st_intersection(range_feature, routes_sf)

  return(as.data.frame(route_range_intersect))

}

pull_range_feature <- function(range_feature_index, range_features) {

  return(range_features[range_feature_index, ])

}


range_ftrs <- drake_plan(
  rng = target(pull_range_feature(range_feature_index, allsp),
                   transform = map(
                     range_feature_index = !!(1:nrow(allsp)))
               ),
   rng_rt = target(get_route_intersections(rng, rl_sf_transformed),
                   transform = map(rng)),
  ar = target(dplyr::combine(rng_rt),
              transform = combine(rng_rt)),
  all_results = target(dplyr::bind_rows(ar))
  )


## Set up the cache and config
db <- DBI::dbConnect(RSQLite::SQLite(), here::here("drake-cache-ranges.sqlite"))
cache <- storr::storr_dbi("datatable", "keystable", db)
cache$del(key = "lock", namespace = "session")


## Run the pipeline
nodename <- Sys.info()["nodename"]
if(grepl("ufhpc", nodename)) {
  print("I know I am on the HiPerGator!")
  library(clustermq)
  options(clustermq.scheduler = "slurm", clustermq.template = "slurm_clustermq.tmpl")
  ## Run the pipeline parallelized for HiPerGator
  make(range_ftrs,
       force = TRUE,
       cache = cache,
       verbose = 1,
       parallelism = "clustermq",
       jobs = 20,
       caching = "main", memory_strategy = "autoclean") # Important for DBI caches!
} else {


  # Run the pipeline on multiple local cores
  system.time(make(range_ftrs, cache = cache,  verbose = 1, memory_strategy = "autoclean", lock_envir = F))


}
