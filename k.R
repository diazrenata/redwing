library(BBSsize)
source(here::here("resim/resim_fxns.R"))

granby = granby

g_gmms <- construct_sampling_gmm(granby, 5)

g_draws <- draw_communities_wrapper(granby, sampling_gmms = g_gmms, ndraws = 100)

library(dplyr)
g_one <- filter(g_draws, sim_iteration == 1)
