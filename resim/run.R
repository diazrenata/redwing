source(here::here("resim/resim_fxns.R"))

ts_comp <- BBSsize::granby

ts_gmms <- construct_sampling_gmm(granby)

gsims <- draw_communities_wrapper(granby, ndraws = 50, sampling_gmms = ts_gmms)

library(ggplot2)

ggplot(gsims, aes(year, total_biomass, color = source, group = as.factor(sim_iteration))) + geom_smooth(se = F, method = "lm") + facet_wrap(vars(source))

ggplot(gsims, aes(year, total_biomass, color = source, group = as.factor(sim_iteration))) + geom_point() + facet_wrap(vars(source))

hartland <- BBSsize::hartland
h_gmms <- construct_sampling_gmm(hartland, begin_years = 1994:1998)

hsims <- draw_communities_wrapper(hartland, ndraws = 50, sampling_gmms = h_gmms, begin_years = 1994:1998)

ggplot(hsims, aes(year, total_biomass, color = sim_iteration, group = as.factor(sim_iteration))) + geom_smooth(se = F, method = "lm") + facet_wrap(vars(source))


library(dplyr)

# this is just some witchcraft so the years line up so I can play with mixed effects/hierarchical models
allsims <- bind_rows(hsims, gsims) %>%
  mutate(year = ifelse(year %in% 1994:1998, year-6, year))

justsims <- filter(allsims, source != "raw")

ggplot(justsims, aes(year, total_energy, group = year, color = source)) + geom_point() + facet_wrap(vars(routename, source), scales = 'free_y')

#### a linear model ####

tlm <- lm(total_energy ~ (timeperiod * source) / routename, data = justsims)

summary(tlm)

# this gives coefficients but they're a headache to interpret.
# also there is no way to account for the fact that years are gonna be correlated with each other
# or for the fact that these are ~abundance data

#### using brms ####
# brm will allow for pulling out a distribution of estimates for quantities of interests, like the estimated effect of sim v actual | whatever route. also random effects.
# brm will be slower.
# brm does not have tweedie implemented. a reasonable alternative might be to log transform prior to running. that will mess a little bit with a straightforward interpretation of slope but that was always going to be a little slippery anyway.

library(brms)
library(tidybayes)

tbrm <- brm(total_energy ~ (timeperiod * source) / routename, data = justsims)

summary(tbrm)

# so each of these parameter estimates, once you get past the base factor levels (which are for granby in this version) is offest from **granby** not from baseline for itself. this is why we want bayesian, so we can get estimates for the slopes relative to route-level baseline.


#' So this requires checking, BUT I think what I have done here is...
#' reconstruct estimates from the posterior for quantities of interest, including: the intercept for each route, the intercept estimated from sims for each route (should be the same as the intercept, we hope!), the "slope" which is really the offset/effect of timeperiod, because timeperiod is modeled as 0 or 1 for the actual, and then the additional effect for the sim. then calculating the values that I'm really interested in, which are 1) the effect of timeperiod for ACTUAL and 2) the effect of timeperiod for SIM. I want to know if those effects are 0, and if they differ from each other. I also want to know the magnitude, so I've calculated something I called an "actual change ratio" which is the offset divided by the intercept. a value of .1 of that means that end is beginning + .1 * beginning.
#' I'm not positive  but I think if you do this with a non-gaussian you want to do all the arithmetic - except possibly the dividing - and then convert back to identity. I'm not sure if you can do the dividing with a non-Gaussian.
#' I'm also not positive if this is simultaneous or instantaneous or whatever but let's be real it's way better than anything else I've ever done in this space.
#' I'm also not positive how this will shake out once you add a random effect of year into the mix.

td <- tidy_draws(tbrm) %>%
  select_at(vars(starts_with("b"))) %>%
  mutate(rowindex = row_number())

td_routes <- td %>%
  select_at(vars(contains("routename"), rowindex)) %>%
  tidyr::pivot_longer(-rowindex, names_to = "term", values_to = "value") %>%
  group_by_all() %>%
  mutate(timeperiod = (unlist(strsplit(term, ":")[1])[[1]]),
         source = (unlist(strsplit(term, ":")[1])[[2]]),
         routename = (unlist(strsplit(term, ":")[1])[[3]])) %>%
  ungroup()  %>%
  group_by_all() %>%
  mutate(si = ifelse(grepl("begin", timeperiod), "intercept", "slope"),
         sa = ifelse(grepl("sim", source), "sim", "actual")) %>%
  mutate(varname = paste0(sa, "_", si)) %>%
  ungroup() %>%
  select(rowindex, value, routename, varname) %>%
  tidyr::pivot_wider(id_cols = c(rowindex, routename), names_from = varname, values_from = value)



td_baseline <- td %>%
  select(b_Intercept, b_timeperiodend, b_sourcesim, `b_timeperiodend:sourcesim`, rowindex) %>%
  mutate(baseline_actual_intercept = b_Intercept,
         baseline_actual_slope = b_timeperiodend,
         baseline_sim_intercept = b_sourcesim,
         baseline_sim_slope = `b_timeperiodend:sourcesim`) %>%
  select(rowindex, baseline_actual_intercept, baseline_actual_slope, baseline_sim_intercept, baseline_sim_slope)

td_route1 <- data.frame (
  rowindex = td_baseline$rowindex,
  actual_intercept = 0,
  actual_slope = 0,
  sim_intercept = 0,
  sim_slope = 0,
  routename = "first")

td_allroutes <- bind_rows(td_routes, td_route1)

td_together <- left_join(td_allroutes, td_baseline)

td_route_ests <- td_together %>%
  group_by_all() %>%
  mutate(
    estimated_actual_intercept = sum(actual_intercept, baseline_actual_intercept),
    estimated_actual_slope = sum(actual_slope, baseline_actual_slope),
    estimated_sim_intercept = sum(sim_intercept, baseline_actual_intercept, baseline_sim_intercept),
    estimated_sim_slope = sum(sim_slope, baseline_sim_slope, baseline_actual_slope),
    estimated_actual_change_ratio = estimated_actual_slope / estimated_actual_intercept,
    estimated_sim_change_ratio = estimated_sim_slope / estimated_sim_intercept
  ) %>%
  ungroup()

ggplot(td_route_ests, aes(estimated_actual_slope)) + geom_density() + geom_density(aes(x = estimated_sim_slope), color = "green") +
  facet_wrap(vars(routename))

ggplot(td_route_ests, aes(estimated_actual_intercept)) + geom_density() + geom_density(aes(x = estimated_sim_intercept), color = "green") +
  facet_wrap(vars(routename))

ggplot(td_route_ests, aes(estimated_actual_change_ratio)) + geom_density() + geom_density(aes(x = estimated_sim_change_ratio), color = "green") +
  facet_wrap(vars(routename))

