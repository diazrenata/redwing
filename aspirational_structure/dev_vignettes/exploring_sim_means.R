library(dplyr)
library(rwar)
library(BBSsize)
library(brms)
library(tidybayes)
library(ggplot2)
theme_set(theme_bw())
library(drake)

## Set up the cache and config
db <- DBI::dbConnect(RSQLite::SQLite(), here::here("aspirational_structure", "drake_caches", "sim_means-cache_biomass_bad.sqlite"))
cache <- storr::storr_dbi("datatable", "keystable", db)
cache$del(key = "lock", namespace = "session")
cached(cache=cache)
loadd(all_comparisons, cache=cache)

# fits_compares <- cached(cache = cache)
# fits_compares <- fits_compares[ which(grepl("fits_compare", fits_compares))]
#
# acs <- list()
#
# for(i in 1:length(fits_compares)) {
#   acs[[i]] <- readd(fits_compares[i], character_only = T, cache = cache)
# }
#
# names(acs) <- fits_compares
#
# all_compares <- bind_rows(acs, .id = "drakename")

winners <- all_comparisons %>%
  #filter(rank == 1) %>%
  # select(matssname, drakename, model, currency, simtype, rank) %>%
  arrange(matssname, currency, simtype, rank) %>%
  mutate(in_one_se = (elpd_diff + se_diff ) >= 0) %>%
  filter(in_one_se)
  filter(simtype == "actual") %>%
  filter(rank < 3)


# how does full_zint compare to full?

loadd(fits_actual_sims_bbs_rtrg_116_18, cache=cache)
all_comparisons %>%
  #filter(rank == 1) %>%
  # select(matssname, drakename, model, currency, simtype, rank) %>%
  arrange(matssname, currency, simtype, rank) %>%
  filter(currency != "biomass") %>%
  filter(simtype == "actual") %>%
  filter(matssname == "bbs_rtrg_116_18") %>%
  mutate(in_one_se = (elpd_diff + se_diff ) >= 0) %>%
  filter(in_one_se)

# for this route, 4 models are in one se. full wins, but nosource is second and within an se. two intercept models are also in one se but i think those should be dropped. intercept_zint reduces to nosource, and _intercept cannot be modeling anything honest.

# anyway, looking at how zint compares to full

zint_draws <- tidy_draws(fits_actual_sims_bbs_rtrg_116_18$te_brms$te_brm_full_zint)
full_draws <- tidy_draws(fits_actual_sims_bbs_rtrg_116_18$te_brms$te_brm_full)

loadd(actual_sims_bbs_rtrg_116_18, cache=cache)
actual_sims_bbs_rtrg_116_18 <- actual_sims_bbs_rtrg_116_18 %>%
  group_by(year, source, timeperiod) %>%
  summarize(total_energy = mean(total_energy)) %>%
  ungroup() %>%
  filter(source != "raw")

ggplot(zint_draws, aes(b_timeperiodend)) + geom_density() + geom_density(data = full_draws, color = "blue")
ggplot(zint_draws, aes(`b_timeperiodend:sourcecurrency`)) + geom_density() + geom_density(data = full_draws, color = "blue")
ggplot(actual_sims_bbs_rtrg_116_18, aes(year, total_energy, color = source)) + geom_point() + geom_smooth(method = "lm", se = F)

# the zint model has more extreme estimates for the slope terms. as expected if, in the other models, the intercept soaks up a little bit of that variation.

ns_draws <- tidy_draws(fits_actual_sims_bbs_rtrg_116_18$te_brms$te_brm_nosource)

si_draws <- tidy_draws(fits_actual_sims_bbs_rtrg_116_18$te_brms$te_brm_intercept_zint)

ggplot(ns_draws, aes(b_timeperiodend)) + geom_density() + geom_density(data = si_draws, color = "blue")

ggplot(ns_draws, aes(b_timeperiodend)) + geom_density() + geom_density(data = zint_draws, color = "blue")


#### trying another dataset ####
all_comparisons %>%
  #filter(rank == 1) %>%
  # select(matssname, drakename, model, currency, simtype, rank) %>%
  arrange(matssname, currency, simtype, rank) %>%
  filter(currency != "biomass") %>%
  filter(simtype == "actual") %>%
  filter(matssname == "bbs_rtrg_224_3") %>%
  mutate(in_one_se = (elpd_diff + se_diff ) >= 0) # This one full wins hands down
loadd(actual_sims_bbs_rtrg_224_3, cache=cache)
actual_sims_bbs_rtrg_224_3 <- actual_sims_bbs_rtrg_224_3 %>%
  group_by(year, source, timeperiod) %>%
  summarize(total_energy = mean(total_energy)) %>%
  ungroup() %>%
  filter(source != "raw")

ggplot(actual_sims_bbs_rtrg_224_3, aes(year, total_energy, color = source)) + geom_point() + geom_smooth(method = "lm", se = F)



#### winners within one se ####

one_se_winners <- all_comparisons %>%
  filter(!grepl("zint", model)) %>%
  filter(!grepl("intercept", model)) %>%
  mutate(model_complexity = ifelse(grepl("full", model), 3, ifelse(grepl("nosource", model), 2, 1))) %>%
  mutate(in_one_se = (elpd_diff + se_diff) >= 0) %>%
  filter(in_one_se) %>%
  group_by(matssname, simtype, currency) %>%
  arrange(model_complexity) %>%
  mutate(model_rank = row_number())
