library(drake)
library(dplyr)
library(ggplot2)
theme_set(theme_bw())
library(rwar)

## Set up the cache and config
db <- DBI::dbConnect(RSQLite::SQLite(), here::here("drake-cache-continentaln.sqlite"))
cache <- storr::storr_dbi("datatable", "keystable", db)
cache$del(key = "lock", namespace = "session")

loadd(all_results, cache = cache)


DBI::dbDisconnect(db)
rm(cache)
rm(db)

db <- DBI::dbConnect(RSQLite::SQLite(), here::here("drake-cache-actual.sqlite"))
cache <- storr::storr_dbi("datatable", "keystable", db)
cache$del(key = "lock", namespace = "session")

all_results_actual <- readd(all_results, cache = cache)


DBI::dbDisconnect(db)
rm(cache)
rm(db)

all_results_actual <- all_results_actual %>%
  filter(routename %in% all_results$routename)

results_summarized <- summarize_null_results(all_results, all_results_actual)

hist(results_summarized$perc_b)
mean(results_summarized$perc_b > .95)
mean(results_summarized$ses_b > 1.96)
