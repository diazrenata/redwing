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

loadd(fits_compare_fits_actual_sims_bbs_rtrg_116_18, cache = cache)
loadd(fits_actual_sims_bbs_rtrg_116_18, cache = cache)

loo_selected <- loo_select(fits_actual_sims_bbs_rtrg_116_18, fits_compare_fits_actual_sims_bbs_rtrg_116_18)
