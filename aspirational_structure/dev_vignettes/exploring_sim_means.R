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

fits_compares <- cached(cache = cache)
fits_compares <- fits_compares[ which(grepl("fits_compare", fits_compares))]

acs <- list()

for(i in 1:length(fits_compares)) {
  acs[[i]] <- readd(fits_compares[i], character_only = T, cache = cache)
}

names(acs) <- fits_compares

all_compares <- bind_rows(acs, .id = "drakename")

winners <- all_compares %>%
  filter(rank == 1) %>%
  select(matssname, drakename, model, currency) %>%
  mutate(simtype =
           ifelse(grepl("actual", drakename), "actual",
                  ifelse(grepl("nsc", drakename), "no size change", "no change"))) %>%
  arrange(matssname, currency, simtype)
