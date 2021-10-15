library(drake)
library(brms)
library(tidybayes)
library(ggplot2)
library(dplyr)
db <- DBI::dbConnect(RSQLite::SQLite(), here::here("drake-cache-actual-resim.sqlite"))
cache <- storr::storr_dbi("datatable", "keystable", db)
cache$del(key = "lock", namespace = "session")

loadd(sims_sgmms_bbs_rtrg_14_33, cache = cache)
some_sims = sims_sgmms_bbs_rtrg_14_33

justsims <- dplyr::filter(some_sims, source %in% c("actual", "sim")) # remove raw

justsims <- justsims %>%
  mutate(total_energy_integer = round(total_energy))

# Fit a brm on total_energy
te_brm <- brms::brm(total_energy ~ (timeperiod * source) + (1 | year), data = justsims, cores = 4, iter = 8000, thin = 2)

summary(te_brm)

loadd(fits_sims_sgmms_bbs_rtrg_23_64, cache = cache)

summary(fits_sims_sgmms_bbs_rtrg_23_64$te_brm)

# Fit the brm on total_biomass
tb_brm <- brms::brm(total_biomass ~ (timeperiod * source) + (1 | year), data = justsims, cores = cores, iter = iter)
