library(dplyr)
library(rwar)
library(drake)
library(MATSS)

expose_imports(rwar)

get_metadat <- function(dataset) {

  metadat <- as.data.frame(dataset$metadata)

  return(metadat)

}

datasets <- MATSS::build_bbs_datasets_plan()

working_datasets <- read.csv(here::here("working_routes.csv"))

datasets <- datasets[ which(datasets$target %in% working_datasets$matssname), ]

methods <- drake_plan(
  m = target(get_metadat(dataset),
             transform = map(
               dataset = !!rlang::syms(datasets$target))),
  md = target(dplyr::bind_rows(m),
              transform= combine(m)))

all = bind_rows(datasets, methods)

## Set up the cache and config
db <- DBI::dbConnect(RSQLite::SQLite(), here::here("bbs_selection", "drake-cache-years.sqlite"))
cache <- storr::storr_dbi("datatable", "keystable", db)
cache$del(key = "lock", namespace = "session")


# Run the pipeline on multiple local cores
system.time(make(all, cache = cache,  verbose = 1, memory_strategy = "autoclean"))





DBI::dbDisconnect(db)
rm(cache)
print("Completed OK")

