library(drake)
library(dplyr)
library(ggplot2)
library(sf)
## Set up the cache and config
db <- DBI::dbConnect(RSQLite::SQLite(), here::here("bbs_selection", "drake-cache-years.sqlite"))
cache <- storr::storr_dbi("datatable", "keystable", db)
cache$del(key = "lock", namespace = "session")
loadd(md, cache = cache)
head(md)

pull_loc <- function(colname) {

  grepl("location.", colname)

}

loc <- md %>%
  select_at(vars(contains("location."))) %>%
  distinct()

ggplot(loc, aes(as.factor(location.bcr))) +
  geom_bar(stat = "count")


#
# ggplot(loc, aes(location.latitude, location.longitude)) + geom_point()
#
# library(tigris)
# all_states <- tigris::states(cb = T)
# all_states <- all_states %>%
#   filter(!(NAME %in% c("Hawaii", "Alaska", "American Samoa", "Guam", "Commonwealth of the Northern Mariana Islands", "United States Virgin Islands", "Puerto Rico")))
#
# ggplot() +
#   geom_sf(data = all_states, size = 1, color = "black", fill = "white")
#
#   cstring <- '+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0'
#
# routes_sf <- st_as_sf(loc, coords = c("location.longitude", "location.latitude"), crs = cstring)
#
# ggplot() + geom_sf(data = routes_sf, aes(color = as.factor(location.bcr))) + theme(legend.position = "none")
#

loc <- loc %>%
  mutate(rown = row_number())

allcomps <- expand.grid(r1 = loc$rown, r2 = loc$rown) %>%
  filter(r1 != r2) %>%
  group_by_all() %>%
  mutate(smaller = min(r1, r2),
         larger = max(r1, r2)) %>%
  ungroup() %>%
  select(smaller, larger) %>%
  distinct() %>%
  left_join(loc, by = c("smaller" = "rown")) %>%
  left_join(loc, by = c("larger" = "rown"))

allcomps <- allcomps %>%
  group_by_all() %>%
  mutate(haver = geosphere::distHaversine(p1 = c(location.longitude.x, location.latitude.x), p2 = c(location.longitude.y, location.latitude.y)))
