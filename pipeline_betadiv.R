library(dplyr)
library(rwar)
library(drake)
library(MATSS)

expose_imports(rwar)


datasets <- MATSS::build_bbs_datasets_plan()


working_datasets <- read.csv(here::here("working_routes.csv"))

datasets <- datasets[ which(datasets$target %in% working_datasets$matssname), ]


methods <- drake_plan(
  s = target(slice_dataset(dataset, startyear = 1988, endyear = 2018),
             transform = map(
               dataset = !!rlang::syms(datasets$target))),
  begin_end_isds = target(get_begin_end_isds(s),
                          transform = map(s)),
  smooths = target(get_begin_end_smooths(begin_end_isds),
                   transform = map(begin_end_isds)),
  spcomp = target(get_begin_end_species(s),
                transform = map(s)),
  all_smooths = target(dplyr::bind_rows(smooths),
                       transform = combine(smooths)),
  all_spcomposition = target(dplyr::combine(spcomp),
                           transform = combine(spcomp))
)

all = bind_rows(datasets, methods)


## Set up the cache and config
db <- DBI::dbConnect(RSQLite::SQLite(), here::here("drake-cache-betadiv.sqlite"))
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
  system.time(make(all, cache = cache,  verbose = 1, memory_strategy = "autoclean"))


}



DBI::dbDisconnect(db)
rm(cache)
print("Completed OK")

