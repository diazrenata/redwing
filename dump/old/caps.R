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
  ungroup() %>%
  group_by_all() %>%
  mutate(meane = totale / totaln,
         meanb = totalb / totaln) %>%
  ungroup()

dc_start <- filter(dc, timeperiod == "begin")

dc_end <- filter(dc, timeperiod == "end")

dc_end_dummy <- dc_end %>%
  left_join(dc_start, by = c("route", "statenum")) %>%
  mutate(totale = totaln.x * meane.y,
         totalb = totaln.x * meanb.y,
         totaln = totaln.x,
         source = 'sim',
         timeperiod = timeperiod.x) %>%
  select(route, statenum, timeperiod, totaln, totale, totalb) %>%
  group_by_all() %>%
  mutate(meane = totale / totaln,
         meanb = totalb / totaln) %>%
  ungroup()

dc_change <- bind_cols(
  select(dc_end, route, statenum),
  dc_end[,4:8] / dc_start[,4:8]
)

dc_change_sim <- bind_cols(
  select(dc_end, route, statenum),
  dc_end_dummy[,4:8] / dc_start[,4:8]
)
