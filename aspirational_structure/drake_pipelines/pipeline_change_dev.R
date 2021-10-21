library(dplyr)
library(rwar)
library(drake)
library(MATSS)
library(BBSsize)
library(brms)
library(tidybayes)

expose_imports(rwar)


datasets <- MATSS::build_bbs_datasets_plan()


working_datasets <- read.csv(here::here("aspirational_structure", "supporting_data", "working_routes.csv"))

datasets <- datasets[ which(datasets$target %in% c("bbs_rtrg_224_3", "bbs_rtrg_318_3", "bbs_rtrg_19_7", "bbs_rtrg_116_18")), ]



methods <- drake_plan(
  sgmms = target(construct_sampling_gmm(dataset, n_isd_draws = 5),
                 transform = map(
                   dataset = !!rlang::syms(datasets$target))),
  sims = target(draw_communities_wrapper(ts_comp = dataset, sampling_gmms = sgmms, ndraws =100),
                transform = map(
                  sgmms
                )),
  as = target(dplyr::combine(sims),
              transform = combine(sims)),
  all_sims = target(dplyr::bind_rows(as)),
  fits = target(fit_brms(sims),
                transform = map(sims)),
  draws = target(get_brm_draws(fits),
                 transform = map(fits)),
  qis = target(get_draw_qis(draws),
               transform = map(draws)),
  ad = target(dplyr::combine(draws),
              transform = combine(draws)),
  all_draws = target(dplyr::bind_rows(ad)),
  allq = target(dplyr::combine(qis),
              transform = map(qis)),
  all_qis = target(dplyr::bind_rows(allq))

)

all = bind_rows(datasets, methods)


## Set up the cache and config
db <- DBI::dbConnect(RSQLite::SQLite(), here::here("aspirational_structure", "drake_caches", "dev-change-cache.sqlite"))
cache <- storr::storr_dbi("datatable", "keystable", db)
cache$del(key = "lock", namespace = "session")

## Run the pipeline
nodename <- Sys.info()["nodename"]
if(grepl("ufhpc", nodename)) {
   print("I know I am on the HiPerGator!")
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
                 lock_envir = F))# Important for DBI caches!
} else {


# Run the pipeline on multiple local cores
 system.time(make(all, cache = cache,  verbose = 1, memory_strategy = "autoclean", lock_envir = F))


}

DBI::dbDisconnect(db)
rm(cache)
print("Completed OK")

