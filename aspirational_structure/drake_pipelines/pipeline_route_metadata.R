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

## Set up the cache and config
db <- DBI::dbConnect(RSQLite::SQLite(), here::here("aspirational_structure", "drake_caches", "drake-cache-route-metadata.sqlite"))
cache <- storr::storr_dbi("datatable", "keystable", db)
cache$del(key = "lock", namespace = "session")


# Run the pipeline on multiple local cores
system.time(make(all, cache = cache,  verbose = 1, memory_strategy = "autoclean"))





DBI::dbDisconnect(db)
rm(cache)
print("Completed OK")

