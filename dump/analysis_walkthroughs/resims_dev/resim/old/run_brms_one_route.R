library(drake)
library(dplyr)
library(brms)


## Set up the cache and config
db <- DBI::dbConnect(RSQLite::SQLite(), here::here("drake-cache-actual-resim-multiprocess.sqlite"))
cache <- storr::storr_dbi("datatable", "keystable", db)
cache$del(key = "lock", namespace = "session")

loadd(all_results, cache= cache)

all_results <- all_results %>%
  mutate(matssname = paste0("bbs_rtrg_", route, "_", statenum)) %>%
  filter(matssname %in% unique(matssname))



DBI::dbDisconnect(db)
rm(cache)
print("Completed OK")

justsims <- filter(all_results, source != "raw")%>%
  filter(matssname %in% unique(matssname)[13]) %>%#13 because I'm listening to taylor swift
  mutate(fyear = as.factor(year))

#short_sims <- filter(justsims, as.numeric(sim_iteration) == 1)

print("starting e short")
print(Sys.time())

# this is for trials on hpg

e_brm_short <- brm(total_energy ~ (timeperiod * source), data = justsims, cores = 1, iter = 2000)

#  e_brm_short <- brm(total_energy ~ (timeperiod * source), data = justsims, cores = 4, chains = 4,iter = 2000, thin = 1)
#
#
#
#  e_brm_re <- brm(total_energy ~ (timeperiod * source) + (1 | (year)), data = justsims, cores = 4, chains = 4,iter = 2000, thin = 1)
#
#
#  e_brm_ref <- brm(total_energy ~ (timeperiod * source) + (1 | (fyear)), data = justsims, cores = 4, chains = 4,iter = 2000, thin = 1)
# #
# #  e_brm_short2 <- brm(total_energy ~ (timeperiod * source) | matssname, data = short_sims, cores = 4, iter = 100, thin = 1)
# #
# # e_lm_short <- lm(total_energy ~ (timeperiod * source * matssname), data = justsims)
#
# print(Sys.time())
#
#
#
#
# save(e_brm_short, file= "e_brm_short_one.Rds")
#
# rm(e_brm_short)
#
# print('e_short')
#
# print("starting b short")
# print(Sys.time())
# b_brm_short <-  brm(total_biomass ~ (timeperiod * source) / matssname, data = short_sims, cores = 4, iter = 2000, thin = 1)
#
# save(b_brm_short, file= "b_brm_short_one.Rds")
# print(Sys.time())
#
# rm(b_brm_short)
#
# print('b_short')

#
# print("starting e full")
# print(Sys.time())
#
# e_brm_full <- brm(total_energy ~ (timeperiod * source) / matssname, data = justsims, cores = 4, iter = 2000, thin = 1)
#
# save(e_brm_full, file= "e_brm_full.Rds")
# print(Sys.time())
#
# rm(e_brm_full)
#
# print('e_full')
# print(Sys.time())
# print("starting b full")
#
# b_brm_full <-  brm(total_biomass ~ (timeperiod * source) / matssname, data = justsims, cores = 4, iter = 2000, thin = 1)
#
# save(b_brm_full, file= "b_brm_full.Rds")
# print(Sys.time())
#
# rm(b_brm_full)
#
# print('b_full')
#
# load("b_brm_full.Rds")
# load('b_brm_short.Rds')
