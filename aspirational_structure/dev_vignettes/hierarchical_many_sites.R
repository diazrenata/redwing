library(dplyr)
library(brms)
# library(drake)
#
#
# ## Set up the cache and config
# db <- DBI::dbConnect(RSQLite::SQLite(), here::here("old_caches", "drake-cache-actual-resim.sqlite"))
# cache <- storr::storr_dbi("datatable", "keystable", db)
# cache$del(key = "lock", namespace = "session")
#
# loadd(all_sims, cache = cache)
#
#
# DBI::dbDisconnect(db)
# rm(cache)
# print("Completed OK")
#
# set.seed(1989)
#
# shorter_sites <- sample(unique(all_sims$matssname), size = 100, replace = F)
#
# shorter_sims <- filter(all_sims, as.numeric(sim_iteration) < 6, matssname %in% shorter_sites, source != "raw")
# write.csv(shorter_sims,here::here("aspirational_structure", "dev_vignettes", "shorter_sims.csv"), row.names = F)

shorter_sims <- read.csv(here::here("aspirational_structure", "dev_vignettes", "shorter_sims.csv"))
set.seed(NULL)

print(Sys.time())
hbrm <- brm(total_energy ~ 0 + ((timeperiod * source )| matssname), data = shorter_sims, cores = 4)

hbrm1 <- brm(total_energy ~ 0 + ((timeperiod * source) | matssname), data = shorter_sims, cores = 4, iter = 10000)

print(Sys.time())

hbrm_post <- posterior_samples(hbrm)
hbrm_post1 <- posterior_samples(hbrm1)

save(hbrm, file = here::here("aspirational_structure", "dev_vignettes", "hbrm.Rds"))
save(hbrm_post, file = here::here("aspirational_structure", "dev_vignettes", "hbrm_post.Rds"))

save(hbrm1, file = here::here("aspirational_structure", "dev_vignettes", "hbrm1.Rds"))
save(hbrm_post1, file = here::here("aspirational_structure", "dev_vignettes", "hbrm_post1.Rds"))
