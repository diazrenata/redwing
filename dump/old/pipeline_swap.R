library(dplyr)
library(rwar)
library(drake)
library(MATSS)

expose_imports(rwar)

be_small = F



datasets <- MATSS::build_bbs_datasets_plan()
nits <- 100

holes <- read.csv(here::here("bbs_holes.csv"))

holes <- filter(holes, region == 18)

datasets <- datasets %>%
  filter(target %in% holes$matssname)

if(be_small) {

  rtrgs <- c("bbs_rtrg_312_17",
             "bbs_rtrg_81_60",
             "bbs_rtrg_203_14",
             "bbs_rtrg_15_59",
             "bbs_rtrg_102_18")

  datasets <- datasets[ which(datasets$target %in% rtrgs), ]

  nits <- 100

}

set.seed(1977)

it_seeds <- sample(100000000, size = nits, replace = F)


shuffled_datasets <-drake_plan(
  s =  target(shuffle_species(dataset, seeds),
                            transform = cross(
                              dataset = !!rlang::syms(datasets$target),
                              seeds = !!it_seeds
                            )))

datasets <- bind_rows(datasets, shuffled_datasets)

methods <- drake_plan(
  begin_end_isds = target(get_begin_end_isds(dataset),
                    transform = map(
                      dataset = !!rlang::syms(datasets$target))),
  smooths = target(get_begin_end_smooths(begin_end_isds),
                        transform = map(begin_end_isds)),
  svs = target(get_begin_end_svs(begin_end_isds),
                   transform = map(begin_end_isds)),
  overlaps = target(overlap(smooths),
                   transform = map(smooths)),
  comp = target(get_begin_end_composition(dataset),
                transform = map(
                  dataset = !!rlang::syms(datasets$target))),
  all_smooths = target(dplyr::bind_rows(smooths),
                        transform = combine(smooths)),
  all_svs = target(dplyr::bind_rows(svs),
                       transform = combine(svs)),
  all_overlaps = target(dplyr::bind_rows(overlaps),
                       transform = combine(overlaps)),
  all_composition = target(dplyr::combine(comp),
                           transform = combine(comp))
)

all = bind_rows(datasets, methods)

if(be_small) {

  ## Set up the cache and config
  db <- DBI::dbConnect(RSQLite::SQLite(), here::here("drake-cache-small.sqlite"))
  cache <- storr::storr_dbi("datatable", "keystable", db)
  cache$del(key = "lock", namespace = "session")
} else {

  ## Set up the cache and config
  db <- DBI::dbConnect(RSQLite::SQLite(), here::here("drake-cache-species-swap.sqlite"))
  cache <- storr::storr_dbi("datatable", "keystable", db)
  cache$del(key = "lock", namespace = "session")

}

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

