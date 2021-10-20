library(BBSsize)
library(rwar)

d <- hartland

d_sampling_gmms <- construct_sampling_gmm(hartland, begin_years = 1994:1998, end_years = 2014:2018)

d_sims <- draw_communities_wrapper(d, begin_years = 1994:1998, sampling_gmms = d_sampling_gmms)

d_sims <- d_sims %>%
  mutate(grouptoplot = paste0(sim_iteration, source))

ggplot(d_sims, aes(year, total_energy, color = source, group = grouptoplot)) + geom_point() + geom_smooth(method = "lm", se = F)

justsims <- d_sims %>%
  filter(source != "raw")


ggplot(justsims, aes(year, total_energy, color = source, group = grouptoplot)) + geom_point() + geom_smooth(method = "lm", se = F)

tb_year <- brm(total_biomass ~  year * source + (1 | year), data = justsims, cores = 4, iter = 4000)

te_year <- brm(total_energy ~  year * source + (1 | year), data = justsims, cores = 4, iter = 4000)


tb_year_draws <- tidybayes::tidy_draws(tb_year)

ggplot(tb_year_draws, aes(b_year)) + geom_histogram()
ggplot(tb_year_draws, aes(`b_year:sourcesim`)) + geom_histogram()



te_year_draws <- tidybayes::tidy_draws(te_year)

ggplot(te_year_draws, aes(b_year)) + geom_histogram()
ggplot(te_year_draws, aes(`b_year:sourcesim`)) + geom_histogram()
