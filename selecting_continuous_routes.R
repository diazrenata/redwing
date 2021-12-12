library(drake)
library(dplyr)
library(ggplot2)
## Set up the cache and config
db <- DBI::dbConnect(RSQLite::SQLite(), here::here("drake_caches", "year_coverage_cache.sqlite"))
cache <- storr::storr_dbi("datatable", "keystable", db)
cache$del(key = "lock", namespace = "session")

loadd(year_coverage, cache=cache)

interval_years <- filter(year_coverage, year %in% c(1988:2018))

route_interval_years <- interval_years %>%
  group_by(route, region) %>%
  summarize(nyears = dplyr::n()) %>%
  ungroup()

ggplot(route_interval_years, aes(nyears)) + geom_histogram()

more_than_x_years <- route_interval_years %>%
  filter(nyears >= 30) %>%
  mutate(matssname = paste0('bbs_rtrg_', route, '_', region))

write.csv(more_than_x_years, file = here::here("supporting_data/perfect_coverage_1988_2018.csv"))
