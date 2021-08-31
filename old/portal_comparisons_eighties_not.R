library(dplyr)
library(ggplot2)
library(rwar)
library(drake)

R.utils::sourceDirectory(here::here("neon_mammals", "fxns"))

sites <- read.csv(here::here("portal_nulls", "eighties_not.csv")) %>%
  select(siteID) %>%
  distinct()

#neon_pairs <- make_pairs(all_neon)

sims <- c(1:100)

setup <- drake::drake_plan(
  mammal_sds = target(read.csv(here::here("portal_nulls", "eighties_not_splist.csv"))),
  portal_pairs = target(data.frame(
    site.x = "control",
    site.y = "exclosure"
  ))
)

real <- drake::drake_plan(
  real_counts = target(read.csv(here::here("portal_nulls", "eighties_not.csv"))))

sim_splists <- drake::drake_plan(
  sim_splist = target(shuffle_neon_species(all_rats_counts = real_counts, shuffle_number = sim_number),
                      transform = map(sim_number = !!sims))
)

shuffled <- drake::drake_plan(
  shuffled_counts = target(change_neon_species(real_counts, shuffled_splist = simlist),
                           transform = map(simlist = !!rlang::syms(sim_splists$target)))
)

all_counts <- bind_rows(real, shuffled)

describe <- drake::drake_plan(
  siteDescs = target(describe_site(siteID, sd_table = mammal_sds, all_rats_counts = ratcounts, max_G = 15),
                     transform = cross(
                       siteID = !!sites$siteID,
                       ratcounts = !!rlang::syms(all_counts$target)
                     )),
  allDescs = target(list(siteDescs),
                    transform = combine(siteDescs, .by = ratcounts)),
  comps = target(make_all_comparisons(pairs_df = portal_pairs, siteDescriptions = allDescs),
                 transform = map(allDescs)),
  allComps = target(bind_rows(comps),
                    transform = combine(comps))
)


plan <- bind_rows(
  setup,
  sim_splists,
  all_counts,
  describe
)



## Set up the cache and config
db <- DBI::dbConnect(RSQLite::SQLite(), here::here("drake-cache-pn-not.sqlite"), synchronous = NULL)
cache <- storr::storr_dbi("datatable", "keystable", db)

cache$del(key = "lock", namespace = "session")


## Run the pipeline
nodename <- Sys.info()["nodename"]
if(grepl("ufhpc", nodename)) {
  print("I know I am on the HiPerGator!")
  library(clustermq)
  options(clustermq.scheduler = "slurm", clustermq.template = "slurm_clustermq.tmpl")
  ## Run the pipeline parallelized for HiPerGator
  make(plan,
       force = TRUE,
       cache = cache,
       verbose = 1,
       parallelism = "clustermq",
       jobs = 20,
       caching = "main", memory_strategy = "autoclean") # Important for DBI caches!
} else {


  # Run the pipeline on multiple local cores
  system.time(make(plan, cache = cache,  verbose = 1, memory_strategy = "autoclean"))


}



DBI::dbDisconnect(db)
rm(cache)
print("Completed OK")
