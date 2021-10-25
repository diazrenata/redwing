library(dplyr)
#library(rwar)
library(drake)
library(MATSS)
library(BBSsize)
library(brms)
library(tidybayes)

source(here::here("aspirational_structure", "dev_vignettes", "sim_means_fxns_dev.R"))

run_hpg = T

datasets <- MATSS::build_bbs_datasets_plan()


working_datasets <- read.csv(here::here("aspirational_structure", "supporting_data", "working_routes.csv"))

datasets <- datasets[ which(datasets$target %in% c("bbs_rtrg_224_3")), ]#, "bbs_rtrg_318_3", "bbs_rtrg_19_7", "bbs_rtrg_116_18")), ]


sim_plan <- drake_plan(
  actual_sims = target(make_actual_sims(dataset),
                        transform = map(
                          dataset = !!rlang::syms(datasets$target)
                        )),
  nc_sims = target(make_nochange_sims(dataset),
                        transform = map(
                          dataset = !!rlang::syms(datasets$target)
                        )),
  nsc_sims = target(make_nosizechange_sims(dataset),
                        transform = map(
                          dataset = !!rlang::syms(datasets$target)
                        ))
)


methods <- drake_plan(
  ssims = target(summarize_sims(sims),
                transform = map(
                  sims = !!rlang::syms(sim_plan$target)
                ) ),
 as = target(dplyr::combine(ssims),
            transform = combine(ssims)),
 all_sims = target(dplyr::bind_rows(as)),
  fits = target(rwar::fit_brms3(sims, iter = 2000, thin = 1),
                transform = map(
                  sims = !!rlang::syms(sim_plan$target)
                )),
  fits_compare = target(rwar::compare_both_brms(fits),
                        transform = map(fits)),
  af = target(dplyr::combine(fits_compare),
              transform = combine(fits_compare)),
  all_comparisons = target(dplyr::bind_rows(af, .id = "drakename"))
)

all = bind_rows(datasets, sim_plan, methods)


## Set up the cache and config
db <- DBI::dbConnect(RSQLite::SQLite(), here::here("aspirational_structure", "drake_caches", "sim_means-cache.sqlite"))
cache <- storr::storr_dbi("datatable", "keystable", db)
cache$del(key = "lock", namespace = "session")

## Run the pipeline
nodename <- Sys.info()["nodename"]
# if(grepl("ufhpc", nodename)) {
#    print("I know I am on the HiPerGator!")

if(run_hpg) {
library(clustermq)
options(clustermq.scheduler = "multicore"#, clustermq.template = "slurm_clustermq.tmpl")
)
## Run the pipeline parallelized for HiPerGator
system.time(make(all,
                 force = TRUE,
                 cache = cache,
                 verbose = 1,
                 parallelism = "clustermq",
                 jobs = 6,
                 caching = "main",
                 memory_strategy = "autoclean",
                 lock_envir = F))# Important for DBI caches!
} else {


# Run the pipeline on multiple local cores
 system.time(make(all, cache = cache,  verbose = 1, memory_strategy = "autoclean", lock_envir = F))


}

DBI::dbDisconnect(db)
rm(cache)
print("Completed OK")

