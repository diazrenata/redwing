library(drake)
library(dplyr)
library(brms)


## Set up the cache and config
db <- DBI::dbConnect(RSQLite::SQLite(), here::here("drake-cache-actual-resim-multiprocess.sqlite"))
cache <- storr::storr_dbi("datatable", "keystable", db)
cache$del(key = "lock", namespace = "session")

loadd(all_results, cache= cache)




DBI::dbDisconnect(db)
rm(cache)
print("Completed OK")

justsims <- filter(all_results, source != "raw")

short_sims <- filter(justsims, as.numeric(sim_iteration) <  50)

print("starting e short")
print(Sys.time())

e_brm_short <- brm(total_energy ~ (timeperiod * source) / matssname, data = short_sims, cores = 4)
print(Sys.time())

save(e_brm_short, "e_brm_short.Rds")

rm(e_brm_short)

print('e_short')

print("starting b short")
print(Sys.time())
b_brm_short <-  brm(total_biomass ~ (timeperiod * source) / matssname, data = short_sims, cores = 4)

save(b_brm_short, "b_brm_short.Rds")
print(Sys.time())

rm(b_brm_short)

print('b_short')


print("starting e full")
print(Sys.time())

e_brm_full <- brm(total_energy ~ (timeperiod * source) / matssname, data = justsims, cores = 4)

save(e_brm_full, "e_brm_full.Rds")
print(Sys.time())

rm(e_brm_full)

print('e_full')
print(Sys.time())
print("starting b full")

b_brm_full <-  brm(total_biomass ~ (timeperiod * source) / matssname, data = justsims, cores = 4)

save(b_brm_full, "b_brm_full.Rds")
print(Sys.time())

rm(b_brm_full)

print('b_full')
