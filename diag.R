library(rwar)
library(BBSsize)
library(dplyr)
library(ggplot2)

d <- MATSS::get_bbs_route_region_data(route = 25, region = 50)
d$covariates$year


d_nochange <- make_nochange_sims(d, n_isd_draws = 10, ndraws = 100)

d_nochange_summ <- summarize_sims(d_nochange)


d_nochange2 <- make_nochange_sims(d, n_isd_draws = 10, ndraws = 100, initial_draw_seed = 10000)


d_nochange3 <- make_nochange_sims(d, n_isd_draws = 10, ndraws = 5, initial_draw_seed = 10000)

d_nochange_summ3<- summarize_sims(d_nochange3)
