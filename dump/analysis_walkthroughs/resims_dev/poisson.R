library(drake)
library(brms)
library(tidybayes)
library(ggplot2)
library(dplyr)
db <- DBI::dbConnect(RSQLite::SQLite(), here::here("drake-cache-actual-resim.sqlite"))
cache <- storr::storr_dbi("datatable", "keystable", db)
cache$del(key = "lock", namespace = "session")

loadd(sims_sgmms_bbs_rtrg_116_18, cache = cache)
some_sims = sims_sgmms_bbs_rtrg_116_18

justsims <- dplyr::filter(some_sims, source %in% c("actual", "sim")) # remove raw

justsims <- justsims %>%
  mutate(total_energy_integer = round(total_energy))

# Fit a brm on total_energy
te_brm <- brms::brm(total_energy_integer ~ (timeperiod * source) + (1 | year), data = justsims, cores = 4, iter = 8000, thin = 2, family = poisson)

te_brm_draws <- tidy_draws(te_brm)
te_brm_fit <- predict(te_brm, re_formula = NA) %>% as.data.frame()

te_brm_fit <- cbind(justsims, te_brm_fit)

ggplot(te_brm_fit, aes(year, total_energy_integer, color = source)) + geom_point() + geom_point(aes(y = Estimate), color = "black") + facet_wrap(vars(source))

loadd(fits_sims_sgmms_bbs_rtrg_116_18, cache = cache)
te_brm_fit_g <- predict(fits_sims_sgmms_bbs_rtrg_116_18$te_brm, re_formula = NA) %>% as.data.frame()
te_brm_fit_g <- cbind(justsims, te_brm_fit_g)


ggplot(te_brm_fit_g, aes(year, total_energy, color = source)) + geom_point() + geom_point(aes(y = Estimate, color = "black")) + facet_wrap(vars(source))


te_brm_sqrt <- brms::brm(sqrt(total_energy) ~ (timeperiod * source) + (1 | year), data = justsims, cores = 4, iter = 8000, thin = 2)

te_brm_sqrt_fit <- predict(te_brm_sqrt, re_formula = NA) %>% as.data.frame()

te_brm_sqrt_fit <- cbind(te_brm_sqrt_fit, justsims)

ggplot(te_brm_sqrt_fit, aes(year, total_energy_integer, color = source)) + geom_point() + geom_point(aes(y = Estimate^2), color = "black") + facet_wrap(vars(source))


ggplot(te_brm_fit_g, aes(year, total_energy_integer, color = source)) + geom_point() + geom_point(aes(y = Estimate), color = "black") + facet_wrap(vars(source))
