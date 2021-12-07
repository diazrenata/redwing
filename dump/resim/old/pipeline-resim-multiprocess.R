library(dplyr)
library(rwar)
library(drake)
library(MATSS)
library(BBSsize)

expose_imports(rwar)
source(here::here("resim/resim_fxns.R"))


datasets <- MATSS::build_bbs_datasets_plan()


working_datasets <- read.csv(here::here("working_routes.csv"))

datasets <- datasets[ which(datasets$target %in% working_datasets$matssname), ]
# datasets <- datasets[1:20, ]


methods <- drake_plan(
  sgmms = target(construct_sampling_gmm(dataset, n_isd_draws = 5),
                   transform = map(
                     dataset = !!rlang::syms(datasets$target))),
  sims = target(draw_communities_wrapper(ts_comp = dataset, sampling_gmms = sgmms, ndraws =100),
                transform = map(
                  sgmms
                )),
  ar = target(dplyr::combine(sims),
              transform = combine(sims)),
  all_results = target(dplyr::bind_rows(ar))
)

all = bind_rows(datasets, methods)


## Set up the cache and config
db <- DBI::dbConnect(RSQLite::SQLite(), here::here("drake-cache-actual-resim-multiprocess.sqlite"))
cache <- storr::storr_dbi("datatable", "keystable", db)
cache$del(key = "lock", namespace = "session")


## Run the pipeline
nodename <- Sys.info()["nodename"]
# if(grepl("ufhpc", nodename)) {
#   print("I know I am on the HiPerGator!")
  library(clustermq)
  options(clustermq.scheduler = "multicore"#, clustermq.template = "slurm_clustermq.tmpl")
  )
  ## Run the pipeline parallelized for HiPerGator
  system.time(make(all,
       force = TRUE,
       cache = cache,
       verbose = 1,
       parallelism = "clustermq",
       jobs = 20,
       caching = "main", memory_strategy = "autoclean") )# Important for DBI caches!
#}# else {
#

  # Run the pipeline on multiple local cores
  # system.time(make(all, cache = cache,  verbose = 1, memory_strategy = "autoclean", lock_envir = F))


#}



DBI::dbDisconnect(db)
rm(cache)
print("Completed OK")

