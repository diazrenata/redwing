#' RMD 10/19/21
#' I think this is redundant and slightly less preferable to pipeline_route_metadata.R
#' Both just iterate over all of the BBS routes in MATSS and pull the metadata.
#' The end result is a big dataframe with a row for every yearxroute that has that year sampled.

library(dplyr)
#library(rwar) # rwar is not actually used in this pipeline
library(drake)
library(MATSS)

#expose_imports(rwar)

get_metadat <- function(dataset) {

  metadat <- as.data.frame(dataset$metadata)

  return(metadat)

}

datasets <- MATSS::build_bbs_datasets_plan()

methods <- drake_plan(
  m = target(get_metadat(dataset),
             transform = map(
               dataset = !!rlang::syms(datasets$target))),
  md = target(dplyr::bind_rows(m),
              transform= combine(m)))

all = bind_rows(datasets, methods)

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

