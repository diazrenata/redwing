library(ggplot2)
library(dplyr)
load(here::here("aspirational_structure/pipelines/portable_comps_all.Rds"))

all_comps_all <- all_comps %>%
  mutate(birds = "all")
load(here::here("aspirational_structure/pipelines/portable_comps_core.Rds"))

all_comps_core <- all_comps %>%
  mutate(birds = "core")

colnames(all_comps_core) <- paste0(colnames(all_comps_core), "_core")

all_comps <- left_join(all_comps_all, all_comps_core, by = c("matssname" = "matssname_core"))


ggplot(all_comps, aes(overlap, overlap_core)) + geom_point()+ geom_abline(slope = 1, intercept = 0)
