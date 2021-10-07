library(dplyr)
library(ggplot2)
library(drake)
theme_set(theme_bw())


## Set up the cache and config
db <- DBI::dbConnect(RSQLite::SQLite(), here::here("drake-cache-actual-caps.sqlite"))
cache <- storr::storr_dbi("datatable", "keystable", db)
cache$del(key = "lock", namespace = "session")

loadd(all_results, cache = cache)

DBI::dbDisconnect(db)
rm(cache)
rm(db)

dc <- all_results %>%
  group_by(route, statenum, timeperiod) %>%
  summarize(totaln = sum(abundance),
            totale = sum(energy),
            totalb = sum(biomass)) %>%
  ungroup()

