pull_location_metadata <- function(a_dataset) {

  return(a_dataset$metadata$location)

}


library(drake)
library(MATSS)
datasets <- MATSS::build_bbs_datasets_plan()


working_datasets <- read.csv(here::here("working_routes.csv"))

datasets <- datasets[ which(datasets$target %in% working_datasets$matssname), ]
# datasets <- datasets[1:5, ]


methods <- drake_plan(
  loc = target(pull_location_metadata(dataset),
                   transform = map(
                     dataset = !!rlang::syms(datasets$target))),
  all_loc = target(dplyr::combine(loc),
              transform = combine(loc)),
  all_df = target(dplyr::bind_rows(all_loc))
)

all = dplyr::bind_rows(datasets, methods)


## Set up the cache and config
db <- DBI::dbConnect(RSQLite::SQLite(), here::here("drake-cache-loc.sqlite"))
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

loadd(all_df, cache= cache)
write.csv(all_df, "analysis_walkthroughs/ranges/route_locations.csv")

DBI::dbDisconnect(db)
rm(cache)
print("Completed OK")

