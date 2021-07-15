library(dplyr)
library(ggplot2)
library(rwar)
library(drake)

R.utils::sourceDirectory(here::here("neon_mammals", "fxns"))
#
# all_neon <- read.csv(here::here("neon_mammals", "provisional_rata_use", "provisional_processed_rata.csv"))
#
# mammal_sds <- read.csv(here::here("neon_mammals", "provisional_rata_use", "provisional_sd_table.csv"))
#
# all_neon_counts <- make_counts(all_neon)

# neon_sites <- unique(all_neon_counts$siteID)
#
# all_neon_sites <- lapply(neon_sites[1:3], FUN = describe_site, sd_table = mammal_sds, all_neon_counts)
#
# names(all_neon_sites) <- neon_sites[1:3]
sites <- read.csv(here::here("neon_mammals", "provisional_rata_use", "provisional_processed_rata.csv")) %>%
  select(siteID) %>%
  distinct()

#neon_pairs <- make_pairs(all_neon)

sims <- c(1:1000)

setup <- drake::drake_plan(
  all_neon = target(read.csv(here::here("neon_mammals", "provisional_rata_use", "provisional_processed_rata.csv"))),
  mammal_sds = target(read.csv(here::here("neon_mammals", "provisional_rata_use", "provisional_sd_table.csv"))),
  neon_pairs = target(make_pairs(all_neon))
)

real <- drake::drake_plan(
  real_counts = target(make_counts(all_neon))
)

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
  siteDescs = target(describe_site(siteID, sd_table = mammal_sds, all_rats_counts = ratcounts),
                     transform = cross(
                       siteID = !!sites$siteID,
                       ratcounts = !!rlang::syms(all_counts$target)
                     )),
  allDescs = target(list(siteDescs),
                    transform = combine(siteDescs, .by = ratcounts)),
  comps = target(make_all_comparisons(pairs_df = neon_pairs, siteDescriptions = allDescs),
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
db <- DBI::dbConnect(RSQLite::SQLite(), here::here("drake-cache-neon.sqlite"))
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
