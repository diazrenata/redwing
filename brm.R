library(BBSsize)
library(rwar)
library(tidyr)
library(dplyr)
library(ggplot2)
d1 <- data.frame(abundance = c(100, 110, 120, 115, 110, 200, 200, 230, 215, 240), biomass = NA, energy = NA, timeperiod = c(rep("begin", times = 5), rep("end", times = 5)), dat = "a") %>%
  mutate(biomass = ( (10 * (timeperiod == "begin")) + 50) * abundance) %>%
  mutate(energy = estimate_b(biomass))

d2 <- data.frame(abundance = c(100, 110, 115, 105, 105, 100, 105, 110, 100, 105) ,biomass = NA, energy = NA, timeperiod = c(rep("begin", times = 5), rep("end", times = 5)), dat = "b") %>%
  mutate(biomass = (50 + (-10 * (timeperiod == "begin"))) * abundance ) %>%
  mutate(energy = estimate_b(biomass))


twodat <- bind_rows(d1, d2) %>%
  pivot_longer(-c(timeperiod, dat), names_to = "currency", values_to = "value")

ggplot(twodat, aes(timeperiod, value)) + geom_point() + facet_grid(rows = vars(dat), cols = vars(currency), scales = "free")


twodat_scaled <- twodat %>%
  group_by(currency, dat) %>%
  mutate(val = scale(sqrt(value))) %>%
  ungroup()

d1_scaled <- filter(twodat_scaled, dat == "a")
d2_scaled <- filter(twodat_scaled, dat == "b")

cap_lm <- lm(val ~ timeperiod * currency, data = d1_scaled)

library(brms)
library(tidybayes)

cap_brm <- brm(val ~ timeperiod * currency, d1_scaled)

get_variables(cap_brm)

ests <- spread_draws(cap_brm, b_timeperiodend,`b_timeperiodend:currencybiomass`, `b_timeperiodend:currencyenergy`)

ggplot(ests, aes(b_timeperiodend)) + geom_density() + geom_vline(xintercept  = 0)

ggplot(ests, aes(`b_timeperiodend:currencybiomass`)) + geom_density() + geom_vline(xintercept  = 0)


ggplot(ests, aes(`b_timeperiodend:currencyenergy`)) + geom_density() + geom_vline(xintercept  = 0)

ggplot(ests, aes(`b_timeperiodend:currencybiomass` + b_timeperiodend)) + geom_density() + geom_vline(xintercept  = 0)


ggplot(ests, aes(`b_timeperiodend:currencyenergy` + b_timeperiodend)) + geom_density() + geom_vline(xintercept  = 0)



cap_brm1 <- brm(val ~ timeperiod * currency, d2_scaled)

get_variables(cap_brm1)

lower95 <- function(vect) {
  return(as.numeric(quantile(vect, .025)))
}
upper95 <- function(vect) {
  return(as.numeric(quantile(vect, .975)))
}

ests1 <- tidybayes::spread_draws(cap_brm1, b_timeperiodend,`b_timeperiodend:currencybiomass`, `b_timeperiodend:currencyenergy`) %>%
  dplyr::mutate(
    slope_abund = b_timeperiodend,
    slope_biomass = b_timeperiodend + `b_timeperiodend:currencybiomass`,
    slope_energy = b_timeperiodend + `b_timeperiodend:currencyenergy`
  ) %>%
  select(-c(.chain, .draw, .iteration)) %>%
  summarize_all(.funs = list(
    mean = mean,
    lower95 = lower95,
    upper95 = upper95
  ))



ggplot(ests1, aes(b_timeperiodend)) + geom_density() + geom_vline(xintercept  = 0)

ggplot(ests1, aes(`b_timeperiodend:currencybiomass`)) + geom_density() + geom_vline(xintercept  = 0)


ggplot(ests1, aes(`b_timeperiodend:currencyenergy`)) + geom_density() + geom_vline(xintercept  = 0)

ggplot(ests1, aes(`b_timeperiodend:currencybiomass` + b_timeperiodend)) + geom_density() + geom_vline(xintercept  = 0)


ggplot(ests1, aes(`b_timeperiodend:currencyenergy` + b_timeperiodend)) + geom_density() + geom_vline(xintercept  = 0)


summary(cap_brm1)
