#' just wondering if lmers will run on this
#' hierarchical brms are not working and I don't think I have the skill and computational options to fix them
#' for some reason adapt_delta does not work on my machine
#'

library(dplyr)
library(drake)
library(lme4)
## Set up the cache and config
db <- DBI::dbConnect(RSQLite::SQLite(), here::here("old_caches", "drake-cache-actual-resim.sqlite"))
cache <- storr::storr_dbi("datatable", "keystable", db)
cache$del(key = "lock", namespace = "session")

loadd(all_sims, cache = cache)

DBI::dbDisconnect(db)
rm(cache)
print("Completed OK")

set.seed(1989)

shorter_sites <- sample(unique(all_sims$matssname), size = 3, replace = F)

shorter_sims <- filter(all_sims, as.numeric(sim_iteration) < 6, matssname %in% shorter_sites)

set.seed(NULL)

print(Sys.time())
hlmer <- lmer(total_energy ~ 0 + ((timeperiod * source) | matssname), data = shorter_sims)


print(Sys.time())

hbrm_post <- posterior_samples(hbrm)
hbrm_post1 <- posterior_samples(hbrm1)

save(hbrm, file = here::here("aspirational_structure", "dev_vignettes", "hbrm.Rds"))
save(hbrm_post, file = here::here("aspirational_structure", "dev_vignettes", "hbrm_post.Rds"))

save(hbrm1, file = here::here("aspirational_structure", "dev_vignettes", "hbrm1.Rds"))
save(hbrm_post1, file = here::here("aspirational_structure", "dev_vignettes", "hbrm_post1.Rds"))
