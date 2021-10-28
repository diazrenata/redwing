library(dplyr)
library(rwar)
library(BBSsize)
library(brms)
library(tidybayes)
library(ggplot2)
theme_set(theme_bw())
library(drake)

load(here::here("aspirational_structure", "pipelines", "portable_results.Rds"))

colnames(all_diagnostics)

summary(all_diagnostics$divergent_sum)

sum(all_diagnostics$neff_b_Intercept < .1)
