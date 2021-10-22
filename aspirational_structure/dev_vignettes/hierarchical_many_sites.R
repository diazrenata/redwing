library(dplyr)
library(drake)
library(brms)

## Set up the cache and config
db <- DBI::dbConnect(RSQLite::SQLite(), here::here("old_caches", "drake-cache-actual-resim.sqlite"))
cache <- storr::storr_dbi("datatable", "keystable", db)
cache$del(key = "lock", namespace = "session")

loadd(all_sims, cache = cache)

DBI::dbDisconnect(db)
rm(cache)
print("Completed OK")

set.seed(1989)

shorter_sites <- sample(unique(all_sims$matssname), size = 100, replace = F)

shorter_sims <- filter(all_sims, as.numeric(sim_iteration) < 6, matssname %in% shorter_sites)

set.seed(NULL)

hbrm <- brm(total_energy ~ 0 + (timeperiod * source | matssname), data = shorter_sims, cores = 4)

hbrm_post <- posterior_samples(hbrm)

save(hbrm, file = "hbrm.Rds")
save(hbrm_post, file = "hbrm_post.Rds")
