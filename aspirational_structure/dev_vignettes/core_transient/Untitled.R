library(BBSsize)
library(rwar)
library(dplyr)
library(ggplot2)

load(here::here("aspirational_structure", "pipelines", "portable_results_core.Rds"))

all_winners_ct <- all_winners %>% mutate(ct = "core")
all_sims_ct <- all_sims %>% mutate(ct = "core")
all_qis_ct <- all_qis %>% mutate(ct= "core")

load(here::here("aspirational_structure", "pipelines", "portable_results_compare.Rds"))
all_winners_all <- all_winners %>% filter(simtype == "actual") %>% mutate(ct = "all")
all_sims_all <- all_sims %>% filter(simtype == "actual") %>% mutate(ct = "all")
all_qis_all <- all_qis %>% filter(simtype == "actual") %>% mutate(ct = "all")


all_winners <- bind_rows(all_winners_ct, all_winners_all) %>%
  select(matssname, currency, model, model_complexity, ct) %>%
  tidyr::pivot_wider(id_cols = c(matssname, currency),
                     names_from = ct,
                     values_from = c(model, model_complexity))


ggplot(all_winners, aes(model_complexity_all, model_complexity_core)) + geom_jitter() + facet_wrap(vars(currency))

filter(all_winners, model_complexity_all < model_complexity_core)

all_sims <- bind_rows(all_sims_all, all_sims_ct)

a_route = filter(all_sims, matssname == "bbs_rtrg_26_68")

ggplot(a_route, aes(year, total_biomass, color = source)) + geom_point() + facet_wrap(vars(ct))
