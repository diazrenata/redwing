library(dplyr)
library(rwar)
library(BBSsize)
library(brms)
library(tidybayes)
library(ggplot2)
theme_set(theme_bw())
dat <- granby
dat_gmms <- construct_sampling_gmm(granby, n_isd_draws = 1)
sims <- draw_communities_wrapper(granby, sampling_gmms = dat_gmms, ndraws = 10)

sims <- sims %>% filter(source != "raw")

ggplot(sims, aes(year, total_energy, color = source)) + geom_point() + geom_smooth(method = "lm", se = F) + facet_wrap(vars(as.factor(sim_iteration))) # you get variation between sims

many_brms <- fit_brms2(sims, cores = 4, iter = 2000, thin = 2)
many_brms_norf <- fit_brms3(sims, cores = 4, iter = 2000, thin = 2)

brm_comparisons <- rwar::compare_both_brms(many_brms)
brm_comparisons_norf <- rwar::compare_both_brms(many_brms_norf)

sims_summarized <- sims %>% group_by(timeperiod, source, year) %>% summarize(total_energy = mean(total_energy), total_biomass = mean(total_biomass)) %>% ungroup()

ggplot(sims_summarized, aes(year, total_energy, color = source)) + geom_jitter() + geom_smooth(method = "lm", se = F) + geom_point(data = sims, alpha = .1)

many_brms_summarized_norf <- fit_brms3(sims_summarized, cores = 4, iter = 2000, thin = 2)

brm_comparisons_summarized_norf <- rwar::compare_both_brms(many_brms_summarized_norf)



#### Exactly the same begin-end ####

# any dynamics picked up here are spurious

granby_nochange <- granby
granby_nochange$abundance[14:18, ] <- granby_nochange$abundance[1:5, ]

dat_nochange <- granby_nochange
dat_nochange_gmms <- rwar::construct_sampling_gmm(dat_nochange, n_isd_draws = 1)
nochange_sims <- rwar::draw_communities_wrapper(dat_nochange, sampling_gmms = dat_nochange_gmms, ndraws = 10)

nochange_sims <- nochange_sims %>% filter(source != "raw")

ggplot(nochange_sims, aes(year, total_energy, color = source)) + geom_point() + geom_smooth(method = "lm", se = F) + facet_wrap(vars(as.factor(sim_iteration))) # you get variation between sims

nochange_many_brms <- rwar::fit_brms2(nochange_sims, cores = 4, iter = 2000, thin = 2)

nochange_norf_brms <- fit_brms3(nochange_sims, cores = 4, iter = 2000, thin = 2)

nochange_brm_comparisons <- rwar::compare_both_brms(nochange_many_brms)

nochange_norf_comparisons <- rwar::compare_both_brms(nochange_norf_brms)

#### Abundance dynamics but no change in ISD ####
library(rwar)
nosizechange_gmms <- dat_gmms
nosizechange_gmms$end <- nosizechange_gmms$begin %>% mutate(timeperiod = "end")

nosizechange_sims <- draw_communities_wrapper(granby, sampling_gmms = nosizechange_gmms, ndraws = 10)

nosizechange_sims <- nosizechange_sims %>% filter(source != "raw")

ggplot(nosizechange_sims, aes(year, total_energy, color = source)) + geom_point() + geom_smooth(method = "lm", se = F) + facet_wrap(vars(as.factor(sim_iteration))) # you get variation between sims

nosizechange_many_brms <- fit_brms2(nosizechange_sims, cores = 4, iter = 2000, thin = 2)

nosizechange_brm_comparisons <- rwar::compare_both_brms(nosizechange_many_brms)
