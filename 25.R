library(dplyr)
library(ggplot2)
library(tidybayes)
library(brms)

someres <- read.csv(here::here("provisional_resim_results.csv"))
colnames(someres)
ggplot(someres, aes(year, total_biomass, group = sim_iteration, color = source)) + geom_point() + facet_wrap(vars(routename))

# routename is not unique but matssname is
someres <- someres %>%
  mutate(matssname = paste0("bbs_rtrg_", route, "_", statenum))

# some exploratory plots
ggplot(filter(someres, matssname == "bbs_rtrg_66_68"), aes(timeperiod, total_biomass, group = sim_iteration, color = source)) + geom_smooth(method = "lm", se = FALSE) + facet_wrap(vars(source))

ggplot(filter(someres, matssname == "bbs_rtrg_66_68"), aes(timeperiod, total_energy, group = sim_iteration, color = source)) + geom_smooth(method = "lm", se = FALSE) + facet_wrap(vars(source))

# k for analyses you want just sims
justsims <- filter(someres, source != "raw")

# one more exploratory plot :)
ggplot(filter(justsims, matssname %in% unique(justsims$matssname)[11:16]), aes(timeperiod, total_energy, group = sim_iteration)) + geom_smooth(method = "lm", se = F) + facet_wrap(vars(matssname, source), ncol = 4)


# running with all the sims was taking ages so trying it this way
short_sims <- filter(justsims, as.numeric(sim_iteration) <  50)

#### brm ####

tbrm <- brm(total_energy ~ (timeperiod * source) / matssname, data = short_sims)

summary(tbrm)
