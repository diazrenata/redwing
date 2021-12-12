#' RMD 10/19/21
#' I think this is redundant and slightly less preferable to pipeline_route_metadata.R
#' Both just iterate over all of the BBS routes in MATSS and pull the metadata.
#' The end result is a big dataframe with a row for every yearxroute that has that year sampled.

library(dplyr)
#library(rwar) # rwar is not actually used in this pipeline
library(drake)
library(MATSS)

#expose_imports(rwar)

get_years <- function(dataset) {

  yeardat <- data.frame(
    year = dataset$covariates$year,
    route = dataset$metadata$route,
    region = dataset$metadata$region,
    location.bcr = dataset$metadata$location$bcr,
    location.statenum = dataset$metadata$location$statenum,
    location.routename = dataset$metadata$location$routename
  )

  return(yeardat)

}

datasets <- MATSS::build_bbs_datasets_plan()


methods <- drake_plan(
  dyears = target(get_years(dataset),
                  transform = map(
                    dataset = !!rlang::syms(datasets$target))),
  year_coverage = target(dplyr::bind_rows(dyears),
                         transform= combine(dyears)))

all = bind_rows(datasets, methods)

## Set up the cache and config
db <- DBI::dbConnect(RSQLite::SQLite(), here::here("drake_caches", "year_coverage_cache.sqlite"))
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
                   jobs = 5,
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

