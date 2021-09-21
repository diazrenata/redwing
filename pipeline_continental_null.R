library(dplyr)
library(rwar)
library(drake)
library(MATSS)
library(BBSsize)

expose_imports(rwar)


datasets <- MATSS::build_bbs_datasets_plan()


working_datasets <- read.csv(here::here("working_routes.csv"))

datasets <- datasets[ which(datasets$target %in% working_datasets$matssname), ]
datasets <- datasets[1:5, ]

nsims = 2

methods <- drake_plan(
  results = target(continental_null_model_wrapper(dataset,  begin_years = c(1988:1992), end_years = c(2014:2018), nsims = !!nsims),
             transform = map(
               dataset = !!rlang::syms(datasets$target))),
  ar = target(dplyr::combine(results),
                           transform = combine(results)),
  all_results = target(dplyr::bind_rows(ar))
)

all = bind_rows(datasets, methods)


## Set up the cache and config
db <- DBI::dbConnect(RSQLite::SQLite(), here::here("drake-cache-continentaln.sqlite"))
cache <- storr::storr_dbi("datatable", "keystable", db)
cache$del(key = "lock", namespace = "session")


## Run the pipeline
nodename <- Sys.info()["nodename"]
if(grepl("ufhpc", nodename)) {
  print("I know I am on the HiPerGator!")
  library(clustermq)
  options(clustermq.scheduler = "slurm", clustermq.template = "slurm_clustermq.tmpl")
  ## Run the pipeline parallelized for HiPerGator
  make(all,
       force = TRUE,
       cache = cache,
       verbose = 1,
       parallelism = "clustermq",
       jobs = 20,
       caching = "main", memory_strategy = "autoclean") # Important for DBI caches!
} else {


  # Run the pipeline on multiple local cores
  system.time(make(all, cache = cache,  verbose = 1, memory_strategy = "autoclean", lock_envir = F))


}



DBI::dbDisconnect(db)
rm(cache)
print("Completed OK")

