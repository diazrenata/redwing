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
  filter(matssname %in% unique(matssname)[1:200])



DBI::dbDisconnect(db)
rm(cache)
print("Completed OK")

justsims <- filter(all_results, source != "raw")

short_sims <- filter(justsims, as.numeric(sim_iteration) <  50)

print("starting e short")
print(Sys.time())

e_brm_short <- brm(total_energy ~ (timeperiod * source) / matssname, data = short_sims, cores = 4, iter = 2000, thin = 1)
print(Sys.time())

save(e_brm_short, file= "e_brm_short.Rds")

rm(e_brm_short)

print('e_short')

print("starting b short")
print(Sys.time())
b_brm_short <-  brm(total_biomass ~ (timeperiod * source) / matssname, data = short_sims, cores = 4, iter = 2000, thin = 1)

save(b_brm_short, file= "b_brm_short.Rds")
print(Sys.time())

rm(b_brm_short)

print('b_short')


print("starting e full")
print(Sys.time())

e_brm_full <- brm(total_energy ~ (timeperiod * source) / matssname, data = justsims, cores = 4, iter = 2000, thin = 1)

save(e_brm_full, file= "e_brm_full.Rds")
print(Sys.time())

rm(e_brm_full)

print('e_full')
print(Sys.time())
print("starting b full")

b_brm_full <-  brm(total_biomass ~ (timeperiod * source) / matssname, data = justsims, cores = 4, iter = 2000, thin = 1)

save(b_brm_full, file= "b_brm_full.Rds")
print(Sys.time())

rm(b_brm_full)

print('b_full')

load("b_brm_full.Rds")
load('b_brm_short.Rds')
