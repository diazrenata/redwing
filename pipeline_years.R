library(dplyr)
library(rwar)
library(drake)
library(MATSS)

expose_imports(rwar)

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
  db <- DBI::dbConnect(RSQLite::SQLite(), here::here("drake-cache-years.sqlite"))
  cache <- storr::storr_dbi("datatable", "keystable", db)
  cache$del(key = "lock", namespace = "session")


  # Run the pipeline on multiple local cores
  system.time(make(all, cache = cache,  verbose = 1, memory_strategy = "autoclean"))





DBI::dbDisconnect(db)
rm(cache)
print("Completed OK")

