library(drake)
library(dplyr)
library(ggplot2)



## Set up the cache and config
db <- DBI::dbConnect(RSQLite::SQLite(), here::here("drake_caches", "all_hasty_toy.sqlite"))
cache <- storr::storr_dbi("datatable", "keystable", db)
cache$del(key = "lock", namespace = "session")


loadd(all_preds, cache=cache)
loadd(all_aics, cache=cache)

winning_aics <- all_aics %>%
  group_by(matssname, model_family) %>%
  mutate(minAICc = min(model_AICc)) %>%
  mutate(deltaAICc = minAICc - model_AICc)  %>%
  mutate(exp_deltas = exp(.5 * deltaAICc)) %>%
  mutate(denom = sum(exp_deltas)) %>%
  mutate(aicc_wt = exp_deltas/ denom) %>%
  arrange(desc(aicc_wt)) %>%
  mutate(rank =row_number()) %>%
  ungroup() %>%
  arrange(matssname, model_family, rank)

delta_2 <- winning_aics %>% filter(deltaAICc > -2) %>%
  group_by(matssname, model_family) %>%
  arrange(modelcomplexity) %>%
  mutate(rank = row_number()) %>%
  filter(rank == 1) %>%
  ungroup()

delta_2 %>% group_by(model_family, model_formula) %>% tally()

winning_fits <- delta_2 %>%
  left_join(all_preds) %>%
  filter(model_family == "Gamma")

winning_fits %>% filter(model_span == 30) %>% group_by(model_family, model_formula) %>% tally() %>% mutate(prop = n / sum(n))

ggplot(filter(winning_fits, modelcomplexity > 1, model_span > 27), aes(ratio_sim)) +
  geom_histogram() +
  geom_vline(xintercept = 1)

summary(filter(winning_fits, modelcomplexity > 1)$ratio_sim)
mean(filter(winning_fits, modelcomplexity > 1)$ratio_sim > 1)


ggplot(filter(winning_fits, modelcomplexity > 2, model_span > 27), aes(ratio_sim, ratio_real)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  geom_abline(intercept = 0, slope = 1) +
  geom_hline(yintercept = 1) +
  geom_vline(xintercept = 1)

mean(filter(winning_fits, modelcomplexity > 2, model_span > 27)$ratio_real >filter(winning_fits, modelcomplexity > 2, model_span > 27)$ratio_sim)

loadd(all_cor_comps, cache=cache)

mean(all_cor_comps$real_cor_percentile < .025)
mean(all_cor_comps$real_cor_ses < 1.95)

long_core_comps <- all_cor_comps %>%
  select(matssname, real_cor, sim_cor_mean) %>%
  tidyr::pivot_longer(-matssname, names_to = "source", values_to = "cor")

ggplot(long_core_comps, aes(source, cor)) + geom_boxplot()

ggplot(all_cor_comps, aes(real_cor)) + geom_histogram()
ggplot(all_cor_comps, aes(sim_cor_mean, real_cor, color = real_cor_percentile < .025)) + geom_point() + geom_abline(slope = 1, intercept = 0) + xlim(-0, 1) + ylim(-.21, 1)
summary(all_cor_comps$real_cor)
summary(filter(all_cor_comps, real_cor_percentile < .05)$real_cor)

all_cor_comps <- left_join(all_cor_comps, winning_fits)

ggplot(all_cor_comps, aes(real_cor, color = model_formula)) + geom_density()
ggplot(all_cor_comps, aes(model_formula, real_cor)) + geom_boxplot()

loadd(all_sims, cache=cache)

cv_sims <- all_sims %>%
  group_by(matssname, source) %>%
  summarize(cv = sd(total_biomass) / mean(total_biomass),
            nvals = dplyr::n()) %>%
  left_join(winning_fits)

ggplot(cv_sims, aes(cv, color = source)) + geom_density() + facet_wrap(vars(model_formula), scales = "free", ncol = 1)
