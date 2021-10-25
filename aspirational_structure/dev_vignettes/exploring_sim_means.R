library(dplyr)
library(rwar)
library(BBSsize)
library(brms)
library(tidybayes)
library(ggplot2)
theme_set(theme_bw())
library(drake)

## Set up the cache and config
db <- DBI::dbConnect(RSQLite::SQLite(), here::here("aspirational_structure", "drake_caches", "sim_means-cache.sqlite"))
cache <- storr::storr_dbi("datatable", "keystable", db)
cache$del(key = "lock", namespace = "session")
cached(cache=cache)
loadd(all_comparisons, cache=cache)

all_comparisons %>% filter(rank == 1) %>% select(currency, matssname, model) %>%
  arrange(matssname, currency)
