library(dplyr)
#library(rwar)
library(drake)
library(MATSS)
library(BBSsize)
library(brms)
library(tidybayes)

run_hpg = T

datasets <- MATSS::build_bbs_datasets_plan()


working_datasets <- read.csv(here::here("aspirational_structure", "supporting_data", "working_routes.csv"))


datasets <- datasets[ which(datasets$target %in% working_datasets$matssname), ]

#datasets <- datasets[ unique(c(1:100, which(datasets$target %in% c("bbs_rtrg_224_3", "bbs_rtrg_318_3", "bbs_rtrg_19_7", "bbs_rtrg_116_18", "bbs_rtrg_3_80")))), ]
#
datasets <- datasets[ which(datasets$target %in% c("bbs_rtrg_224_3", "bbs_rtrg_318_3", "bbs_rtrg_19_7", "bbs_rtrg_116_18", "bbs_rtrg_3_80")), ]

#datasets <- datasets[ which(datasets$target %in% c("bbs_rtrg_116_18", "bbs_rtrg_318_3")), ]

#
# sim_plan <- drake_plan(
#   actual_sims = target(rwar::ssims_wrapper(dataset, simtype = "actual"),
#                        transform = map(
#                          dataset = !!rlang::syms(datasets$target)
#                        )),
#   nc_sims = target(rwar::ssims_wrapper(dataset, simtype = "nc"),
#                    transform = map(
#                      dataset = !!rlang::syms(datasets$target)
#                    )),
#   nsc_sims = target(rwar::ssims_wrapper(dataset, simtype = "nsc"),
#                     transform = map(
#                     ))
# )


methods <- drake_plan(
  lnm = target(rwar::local_null_model_wrapper(dataset, n_null_model_sims = 500, n_isd_draws = 5, ndraws = 25, initial_null_model_seed = 1989),
                 transform = map(
                   dataset = !!rlang::syms(datasets$target)
                 )),
  lnm_s = target(rwar::summarize_null_models(lnm),
                 transform = map(lnm)),
  anm = target(rwar::local_null_model_wrapper(dataset, n_null_model_sims = 1, n_isd_draws = 5, ndraws = 25, initial_null_model_seed = 1989, return_actual = T),
               transform = map(
                 dataset = !!rlang::syms(datasets$target)
               )),
  anm_s = target(rwar::summarize_null_models(anm),
                 transform = map(anm)),
  as = target(dplyr::combine(lnm_s),
              transform = combine(lnm_s)),
  all_summaries = target(dplyr::bind_rows(as)),
  aa = target(dplyr::combine(anm_s),
              transform = combine(anm_s)),
  all_actual_summaries = target(dplyr::bind_rows(aa))
)

all = bind_rows(datasets, methods)


## Set up the cache and config
db <- DBI::dbConnect(RSQLite::SQLite(), here::here("aspirational_structure", "drake_caches", "dev_lnm.sqlite"))
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
                   jobs = 5,
                   caching = "main",
                   memory_strategy = "autoclean",
                   lock_envir = F,
                   garbage_collection = T))# Important for DBI caches!
} else {


  # Run the pipeline on multiple local cores
  system.time(make(all, cache = cache,  verbose = 1, memory_strategy = "autoclean", lock_envir = F))


}

loadd(all_summaries, cache = cache)
save(all_summaries, file = "local_summaries.Rds")

DBI::dbDisconnect(db)
rm(cache)
print("Completed OK")

