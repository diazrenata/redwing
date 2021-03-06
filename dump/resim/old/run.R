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

coe <- data.frame(
  term = names(coef(tlm)),
  value = as.vector(coef(tlm))
) %>%
  mutate(description = c(
    "baseline_actual_intercept",
    "baseline_actual_change",
    "baseline_sim_intercept",
    "baseline_sim_change",
    "actual_intercept",
    "actual_change",
    "sim_intercept",
    "sim_change"
  )) %>%
  select(value, description) %>%
  tidyr::pivot_wider(everything(), names_from = description, values_from = value)


predvals <- justsims %>%
  mutate(preds= predict(tlm)) %>%
  select(timeperiod, routename, source, preds) %>%
  distinct()

# baseline actual intercept should be begin actual granby
predvals %>% filter(timeperiod == "begin", source == "actual", routename == "GRANBY")
coe$baseline_actual_intercept

# end actual granby should be baseline actual intercept + baseline actual change
predvals %>% filter(timeperiod == "end", source == "actual", routename == "GRANBY")
coe$baseline_actual_intercept + coe$baseline_actual_change

# begin sim granby should be baseline actual intercept + baseline sim intercept
predvals %>% filter(timeperiod == "begin", source == "sim", routename == "GRANBY")
coe$baseline_actual_intercept + coe$baseline_sim_intercept

# end sim granby should be baseline actual intercept + baseline sim intercept + baseline sim change + baseline actual change
predvals %>% filter(timeperiod == "end", source == "sim", routename == "GRANBY")
coe$baseline_actual_intercept + coe$baseline_sim_intercept + coe$baseline_sim_change + coe$baseline_actual_change

# begin actual hartland should be baseline actual intercept + actual intercept
predvals %>% filter(timeperiod == "begin", source == "actual", routename == "NEW HARTFORD")
coe$baseline_actual_intercept + coe$actual_intercept

# end actual hartland should be baseline actual intercept + baseline actual change + actual change
# !!!!
predvals %>% filter(timeperiod == "end", source == "actual", routename == "NEW HARTFORD")
coe$baseline_actual_intercept + coe$baseline_actual_change + coe$actual_change

# begin sim hartland should be baseline actual intercept + sim intercept + baseline sim intercept
predvals %>% filter(timeperiod == "begin", source == "sim", routename == "NEW HARTFORD")
coe$baseline_actual_intercept + coe$sim_intercept + coe$baseline_sim_intercept

# end sim hartland should be baseline actual intercept  + sim intercept + sim change
predvals %>% filter(timeperiod == "end", source == "sim", routename == "NEW HARTFORD")
coe$baseline_actual_intercept + coe$baseline_actual_change + coe$sim_change + coe$baseline_sim_change + coe$baseline_sim_intercept



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



# Get all draws from the posterior and get just the terms we want
td <- tidy_draws(tbrm) %>%
  select_at(vars(starts_with("b"))) %>%
  mutate(rowindex = row_number()) # and get a row index to keep draws together, I'm not sure if this matters but I'll do it


# Some nightmare parsing to get the variable names into something more intuitive and that won't need ``s
# For just the by-route offsets in a nested model of the form value ~ (timeperiod * source) / route
# You'll need to change `routename` to `matssname` because there are duplicate route names in the full dataset
# All this is necessary because the column names contain routename. There's probably a simpler way to do this with spread_draws but I don't get the variable selection/nesting syntax so doing it manually.
td_routes <- td %>%
  select_at(vars(contains("routename"), rowindex)) %>% # get the terms involved in route-level offsets from the baseline (i.e. the values for the first route)
  tidyr::pivot_longer(-rowindex, names_to = "term", values_to = "value") %>%
  group_by_all() %>%
  mutate(timeperiod = (unlist(strsplit(term, ":")[1])[[1]]), # the first piece of the variable name tells you begin/end
         source = (unlist(strsplit(term, ":")[1])[[2]]), # second sim/actual
         routename = (unlist(strsplit(term, ":")[1])[[3]])) %>% # third routename
  ungroup()  %>%
  group_by_all() %>%
  mutate(si = ifelse(grepl("begin", timeperiod), "intercept", "change"), # all the "begin" terms are the intercept offsets for each route. if not "begin", is "end", or the amount of change we add (to the baseline change) for this route.
         sa = ifelse(grepl("sim", source), "sim", "actual")) %>% # sim and actual terms. at this level the bifurcate.
  mutate(varname = paste0(sa, "_", si)) %>%
  ungroup() %>%
  select(rowindex, value, routename, varname) %>%
  tidyr::pivot_wider(id_cols = c(rowindex, routename), names_from = varname, values_from = value) # scoot the values over to new names


# Baseline values - to get the actual estimates for each route, you have to add the route-level estimates to the baseline estimates. Renaming these to intuitive things.
td_baseline <- td %>%
  select(b_Intercept, b_timeperiodend, b_sourcesim, `b_timeperiodend:sourcesim`, rowindex) %>%
  mutate(baseline_actual_intercept = b_Intercept,
         baseline_actual_change = b_timeperiodend,
         baseline_sim_intercept = b_sourcesim,
         baseline_sim_change = `b_timeperiodend:sourcesim`) %>%
  select(rowindex, baseline_actual_intercept, baseline_actual_change, baseline_sim_intercept, baseline_sim_change)

# The first route has its route-level values estimated as the baseline values. To get estimates for it, creating a dummy dataframe with route-level parameters set to zero.

td_route1 <- data.frame (
  rowindex = td_baseline$rowindex,
  actual_intercept = 0,
  actual_change = 0,
  sim_intercept = 0,
  sim_change = 0,
  routename = "first")

# Then binding the dummy dataframe for the baseline route to the route-level estimates for all the other routes
td_allroutes <- bind_rows(td_routes, td_route1)


# Then sticking the route level estimates to the baseline values
td_together <- left_join(td_allroutes, td_baseline)

# And doing the arithmetic to get to various quantities of interest.
# Estimate for every draw from the posterior for every route.
# There's something wrong with this arithmetic, I'm not adding the right things together.

#### use this to sort out the arithmetic

tlm <- lm(total_energy ~ (timeperiod * source) / routename, data = justsims)

summary(tlm)

coe <- data.frame(
  term = names(coef(tlm)),
  value = as.vector(coef(tlm))
) %>%
  mutate(description = c(
    "baseline_actual_intercept",
    "baseline_actual_change",
    "baseline_sim_intercept",
    "baseline_sim_change",
    "actual_intercept",
    "actual_change",
    "sim_intercept",
    "sim_change"
  )) %>%
  select(value, description) %>%
  tidyr::pivot_wider(everything(), names_from = description, values_from = value)


predvals <- justsims %>%
  mutate(preds= predict(tlm)) %>%
  select(timeperiod, routename, source, preds) %>%
  distinct()

# baseline actual intercept should be begin actual granby
predvals %>% filter(timeperiod == "begin", source == "actual", routename == "GRANBY")
coe$baseline_actual_intercept

# end actual granby should be baseline actual intercept + baseline actual change
predvals %>% filter(timeperiod == "end", source == "actual", routename == "GRANBY")
coe$baseline_actual_intercept + coe$baseline_actual_change

# begin sim granby should be baseline actual intercept + baseline sim intercept
predvals %>% filter(timeperiod == "begin", source == "sim", routename == "GRANBY")
coe$baseline_actual_intercept + coe$baseline_sim_intercept

# end sim granby should be baseline actual intercept + baseline sim intercept + baseline sim change + baseline actual change
predvals %>% filter(timeperiod == "end", source == "sim", routename == "GRANBY")
coe$baseline_actual_intercept + coe$baseline_sim_intercept + coe$baseline_sim_change + coe$baseline_actual_change

# begin actual hartland should be baseline actual intercept + actual intercept
predvals %>% filter(timeperiod == "begin", source == "actual", routename == "NEW HARTFORD")
coe$baseline_actual_intercept + coe$actual_intercept

# end actual hartland should be baseline actual intercept + baseline actual change + actual change
# !!!!
predvals %>% filter(timeperiod == "end", source == "actual", routename == "NEW HARTFORD")
coe$baseline_actual_intercept + coe$baseline_actual_change + coe$actual_change

# begin sim hartland should be baseline actual intercept + sim intercept + baseline sim intercept
predvals %>% filter(timeperiod == "begin", source == "sim", routename == "NEW HARTFORD")
coe$baseline_actual_intercept + coe$sim_intercept + coe$baseline_sim_intercept

# end sim hartland should be baseline actual intercept  + sim intercept + sim change
predvals %>% filter(timeperiod == "end", source == "sim", routename == "NEW HARTFORD")
coe$baseline_actual_intercept + coe$baseline_actual_change + coe$sim_change + coe$baseline_sim_change + coe$baseline_sim_intercept

####
td_route_ests <- td_together %>%
  group_by_all() %>%
  mutate(
    estimated_actual_begin = sum(actual_intercept, baseline_actual_intercept), # estimated beginning value
    estimated_actual_end = sum(actual_change, baseline_actual_change, baseline_actual_intercept), # estimated end value
    estimated_sim_begin = sum(sim_intercept, baseline_actual_intercept, baseline_sim_intercept), # estimated beginning value from sims. we expect this to be equal to the estimated beginning value, any change is just sampling error.
    estimated_sim_end = sum(baseline_actual_intercept, baseline_actual_change, baseline_sim_change, baseline_sim_intercept, sim_change),
    estimated_actual_change_ratio = (estimated_actual_end - estimated_actual_begin) / estimated_actual_begin, # this is a measure of the magnitude of the change from beginning to end. the sign is going to be increase (positive) or decrease. the magnitude is the % increase. so .1 = added 10% of starting (biomass or energy) to get to the end. -.2 = lost 20% of starting (biomass or energy) between begin and end.
    estimated_sim_change_ratio = (estimated_sim_end - estimated_sim_begin) / estimated_sim_begin, # same measure but having drawn the end values using the beginning isd. this is the amount of change expected due only to changes in the numbers of individuals observed in each time period. by comparing estimated_actual_change_ratio to estimated_sim_change_ratio, I believe we get an estimate of both the significance and magnitude of decoupling of (biomass or energy) and numerical abundance due to changes in the size spectrum.
    estimated_actual_change = estimated_actual_end - estimated_actual_begin, # the "slope" assuming x = 0 or 1 for begin or end.
    estimated_sim_change = estimated_sim_end - estimated_sim_begin
  ) %>%
  ungroup()


ggplot(td_route_ests, aes(estimated_actual_begin)) + geom_density() + geom_density(aes(x = estimated_sim_begin), color = "green") +
  facet_wrap(vars(routename))

ggplot(td_route_ests, aes(estimated_actual_end)) + geom_density() + geom_density(aes(x = estimated_sim_end), color = "green") +
  facet_wrap(vars(routename))


ggplot(td_route_ests, aes(estimated_actual_change)) + geom_density() + geom_density(aes(x = estimated_sim_change), color = "green") +
  facet_wrap(vars(routename))


ggplot(td_route_ests, aes(estimated_actual_change_ratio)) + geom_density() + geom_density(aes(x = estimated_sim_change_ratio), color = "green") +
  facet_wrap(vars(routename))


lower_quantile <- function(vector) {
  as.numeric(quantile(vector, probs = .025))
}
upper_quantile <- function(vector) {
  as.numeric(quantile(vector, probs = .975))
}

td_route_ests_summary <- td_route_ests %>%
  select(-rowindex) %>%
  group_by(routename) %>%
  summarize_all(.funs = list(mean = mean,
                             lower = lower_quantile,
                             upper = upper_quantile)) %>%
  ungroup()


ggplot(td_route_ests_summary, aes(estimated_sim_change_ratio_mean, estimated_actual_change_ratio_mean, color= routename)) +
  geom_point() +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0)+
  geom_abline(intercept = 0, slope = 1) +
  geom_errorbarh(aes(xmin = estimated_sim_change_ratio_lower, xmax = estimated_sim_change_ratio_upper, y = estimated_actual_change_ratio_mean), height = .005) +
  geom_errorbar(aes(ymin = estimated_actual_change_ratio_lower, ymax = estimated_actual_change_ratio_upper, x = estimated_sim_change_ratio_mean), width = .005) +
  scale_color_viridis_d(option='mako', begin = .2, end =.8)



ggplot(td_route_ests_summary, aes(estimated_sim_change_mean, estimated_actual_change_mean, color= routename)) +
  geom_point() +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0)+
  geom_abline(intercept = 0, slope = 1) +
  geom_errorbarh(aes(xmin = estimated_sim_change_lower, xmax = estimated_sim_change_upper, y = estimated_actual_change_mean), height = .005) +
  geom_errorbar(aes(ymin = estimated_actual_change_lower, ymax = estimated_actual_change_upper, x = estimated_sim_change_mean), width = .005) +
  scale_color_viridis_d(option='mako', begin = .2, end =.8)


# I bet you can do this with predict............

dummy_newdat <- predvals %>%
  select(-preds)

ests_sim <- replicate(4000, mutate(dummy_newdat, pred = predict(tbrm, newdata = dummy_newdat)), simplify = F)
ests_sim_raw <- ests_sim

names(ests_sim) <- 1:4000
ests_sim <- bind_rows(ests_sim, .id = "sim_id")

# and go from there

ests_sim$pred <- ests_sim[,"pred"]$pred[,1]

ests_sim_wide <- tidyr::pivot_wider(ests_sim, id_cols = c(sim_id, routename), names_from = c(source, timeperiod), values_from = pred)

ggplot(ests_sim_wide, aes(actual_begin)) + geom_density() + geom_density(aes( sim_begin), color = "green") + facet_wrap(vars(routename), scales = "free")

ggplot(ests_sim_wide, aes(actual_end)) + geom_density() + geom_density(aes( sim_end), color = "green") + facet_wrap(vars(routename), scales = "free")

ests_sim_wide <- ests_sim_wide %>%
  mutate(actual_change = actual_end - actual_begin,
         sim_change = sim_end - sim_begin,
         sim_offset = sim_begin - actual_begin,
         actual_change_ratio = actual_change / actual_begin,
         sim_change_ratio = sim_change / sim_begin,
         routename = ifelse(routename == "GRANBY", "first", "routenameNEWHARTFORD"))

ggplot(ests_sim_wide, aes(actual_change_ratio)) + geom_density() + facet_wrap(vars(routename)) + geom_density(aes(sim_change_ratio), color = "green")


ggplot(ests_sim_wide, aes(sim_offset)) + geom_density() + facet_wrap(vars(routename))


ggplot(ests_sim_wide, aes(actual_change_ratio)) + geom_density() + facet_wrap(vars(routename), scales = "free") + geom_density(data = td_route_ests, aes(x = estimated_actual_change_ratio), color = "green")


ggplot(ests_sim_wide, aes(actual_change)) + geom_density() + facet_wrap(vars(routename), scales = "free") + geom_density(data = td_route_ests, aes(x = estimated_actual_change), color = "green")
# using the posterior draws - rather than the estimate from predict - is wider.

# I suspect this is because predict is doing some level of sampling + averaging.
# I am inclined to use the draws.


