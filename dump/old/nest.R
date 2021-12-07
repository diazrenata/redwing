library(BBSsize)
library(rwar)
library(nlme)
library(dplyr)
library(ggplot2)

library(tidyr)
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
  mutate(value = scale(sqrt(value))) %>%
  ungroup()


ggplot(twodat_scaled, aes(timeperiod, value)) + geom_point() + facet_grid(rows = vars(dat), cols = vars(currency), scales = "free")

try <- lm(value ~ (timeperiod * currency * dat), twodat_scaled)

summary(try)

coefficients(try)

library(emmeans)
emm_try <- emmeans(try2, specs = ~ (timeperiod) | (currency + dat))
emm_try
pairs(emm_try)

library(brms)

try_brm <- brm(value ~ timeperiod * currency * dat, data = twodat_scaled)


