library(ggplot2)
library(dplyr)
load(here::here("aspirational_structure/pipelines/portable_comps_all.Rds"))

all_comps_all <- all_comps %>%
  mutate(birds = "all")
load(here::here("aspirational_structure/pipelines/portable_comps_core.Rds"))

all_comps_core <- all_comps %>%
  mutate(birds = "core")

colnames(all_comps_core) <- paste0(colnames(all_comps_core), "_core")

load(here::here("aspirational_structure/pipelines/portable_comps_transient.Rds"))

all_comps_transient <- all_comps %>%
  mutate(birds = "transient")

colnames(all_comps_transient) <- paste0(colnames(all_comps_transient), "_transient")


all_comps <- left_join(all_comps_all, all_comps_core, by = c("matssname" = "matssname_core")) %>%
  left_join(all_comps_transient, by = c("matssname" = "matssname_transient"))


ggplot(all_comps, aes(overlap, overlap_core)) + geom_point()+ geom_abline(slope = 1, intercept = 0)

all_comps <- all_comps %>%
  mutate(transient_size_ratio = end_mean_size_transient / begin_mean_size_transient,
         core_size_ratio = end_mean_size_core / begin_mean_size_core,
         all_size_ratio = end_mean_size / begin_mean_size
         )

ggplot(all_comps, aes(log(transient_size_ratio))) + geom_histogram()
ggplot(all_comps, aes(log(core_size_ratio))) + geom_histogram()
ggplot(all_comps, aes(log(all_size_ratio))) + geom_histogram()

summary(all_comps$transient_size_ratio)
summary(all_comps$all_size_ratio)
summary(all_comps$core_size_ratio)

ggplot(all_comps, aes(all_size_ratio, transient_size_ratio)) + geom_point() + scale_x_log10() + scale_y_log10() + geom_abline(slope = 1, intercept = 0)


a_weird_route <- MATSS::get_bbs_route_region_data(route = 119, region = 68)

weird_transients <- rwar::just_transient(a_weird_route, T)

an_isd <- rwar::just_isd(weird_transients, 1989)

begin_isd <- filter(an_isd, year %in% 1988:1992)
end_isd <- filter(an_isd, year %in% 2014:2018)

median(begin_isd$mass)
median(end_isd$mass)

begin_ecdf <- ecdf(begin_isd$mass)


ggplot(begin_isd, aes(log(mass))) + geom_density() + xlim(0, 15)  + geom_density(data = end_isd, color = "blue")

ggplot(end_isd, aes(log(mass))) + geom_density() + xlim(0, 15)

