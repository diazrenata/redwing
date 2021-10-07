source(here::here("resim/resim_fxns.R"))

ts_comp <- BBSsize::granby

ts_gmms <- construct_sampling_gmm(granby)

gsims <- draw_communities_wrapper(granby, ndraws = 50, sampling_gmms = ts_gmms)

library(ggplot2)

ggplot(gsims, aes(year, total_biomass, color = source, group = as.factor(sim_iteration))) + geom_smooth(se = F, method = "lm") + facet_wrap(vars(source))

ggplot(gsims, aes(year, total_biomass, color = source, group = as.factor(sim_iteration))) + geom_point() + facet_wrap(vars(source))

hartland <- BBSsize::hartland
h_gmms <- construct_sampling_gmm(hartland, begin_years = 1994:1998)

hsims <- draw_communities_wrapper(hartland, ndraws = 2, sampling_gmms = h_gmms, begin_years = 1994:1998)

ggplot(hsims, aes(year, total_biomass, color = sim_iteration, group = as.factor(sim_iteration))) + geom_smooth(se = F, method = "lm") + facet_wrap(vars(source))
