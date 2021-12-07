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

#datasets <- datasets[ unique(c(1:250, which(datasets$target %in% c("bbs_rtrg_224_3", "bbs_rtrg_318_3", "bbs_rtrg_19_7", "bbs_rtrg_116_18", "bbs_rtrg_3_80")))), ]
#
#datasets <- datasets[ which(datasets$target %in% c("bbs_rtrg_224_3", "bbs_rtrg_318_3", "bbs_rtrg_19_7", "bbs_rtrg_116_18", "bbs_rtrg_3_80")), ]

# datasets <- datasets[ which(datasets$target %in% c("bbs_rtrg_116_18")), ]

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
#
# draw_wrapper <- function(winners, fits) {
#   draws = rwar::winner_draws(winners, fits)
#   draw_qis = rwar::winner_qis(draws)
#   draw_qis
# }

methods <- drake_plan(
  coredat = target(rwar::core_transient(dataset, core_only = T),
                   transform = map(
                     dataset = !!rlang::syms(datasets$target)
                   )),
  comps = target(rwar::be_comparison(coredat),
                 transform = map(
                   coredat
                 ),
                 trigger = trigger(condition = T)),
  ac = target(dplyr::combine(comps),
             transform = combine(comps)),
  all_comps = target(dplyr::bind_rows(ac))
)

all = bind_rows(datasets, methods)


## Set up the cache and config
db <- DBI::dbConnect(RSQLite::SQLite(), here::here("aspirational_structure", "drake_caches", "comps_core.sqlite"))
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
                   jobs = 4,
                   caching = "main",
                   memory_strategy = "autoclean",
                   lock_envir = F,
                   garbage_collection = T))# Important for DBI caches!
} else {


  # Run the pipeline on multiple local cores
  system.time(make(all, cache = cache,  verbose = 1, memory_strategy = "autoclean", lock_envir = F))


}
#
 loadd(all_comps, cache = cache)
 save(all_comps, file = "portable_comps_core.Rds")

DBI::dbDisconnect(db)
rm(cache)
print("Completed OK")

