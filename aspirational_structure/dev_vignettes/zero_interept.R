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
loadd(actual_sims_bbs_rtrg_318_3, cache=cache)

justsims <- filter(actual_sims_bbs_rtrg_318_3, source != "raw") %>%
  group_by(year, source, timeperiod) %>%
  summarize(total_biomass = mean(total_biomass)) %>%
  ungroup()

a_brm <- brm(total_biomass ~ timeperiod * source, data = justsims, prior = prior(constant(0), class = "b", coef = "sourcecurrency") )
