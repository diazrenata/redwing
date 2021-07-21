library(dplyr)
library(MATSS)
library(ggplot2)
library(rwar)
library(drake)

R.utils::sourceDirectory(here::here("neon_mammals", "fxns"))

sites <- read.csv(here::here("working_routes_max10.csv"))[1:3,]

sims <- c(1:2)

datasets <- MATSS::build_bbs_datasets_plan()

datasets <- datasets[ which(datasets$target %in% sites$matssname), ]

bp <- drake::drake_plan(
  bird_pairs = target(make_bird_pairs_temporal(sites))
)

not_datasets <- drake::drake_plan(
  nt = target(remove_transients(matss_dataset = ds),
              transform = map(ds = !!rlang::syms(datasets$target)))
)

pulled_datasets <- drake::drake_plan(
  d = target(pull_bird_dat_temporal(matss_dataset = ds, timechunk = tc),
             transform = cross(ds = !!rlang::syms(not_datasets$target),
                               tc = c("start", "end")))
)

real <- drake::drake_plan(
  real_counts = target(get_bird_counts(matss_dataset = ds),
                       transform = map(ds = !!rlang::syms(pulled_datasets$target)))
)

real_splists <- drake::drake_plan(
  sp = target(pull_bird_sp(matss_dataset = ds),
              transform = map(ds = !!rlang::syms(pulled_datasets$target))),
  all_sp = target(dplyr::distinct(dplyr::bind_rows(sp)),
                  transform = combine(sp))
)

sim_splists <- drake::drake_plan(

  sim_splist = target(shuffle_bird_species(all_bird_spp = all_sp, shuffle_number = sim_number),
                      transform = map(sim_number = !!sims))
)



real_describe <- drake::drake_plan(
  siteDescs = target(describe_bird_counts_temporal(bird_counts = bc, all_bird_spp = all_sp),
                     transform = map(bc = !!rlang::syms(real$target))),
  allDescs = target(list(siteDescs),
                    transform = combine(siteDescs)),
  comps = target(make_all_bird_comparisons(bird_pairs, siteDescriptions = allDescs)))

sim_describe <- drake::drake_plan(
  siteDescs = target(describe_bird_counts_temporal(bird_counts = bc, shuffled_sp = all_sp, all_bird_spp = all_sp),
                     transform = cross(bc = !!rlang::syms(real$target),
                                       all_sp = !!rlang::syms(sim_splists$target))),
  allDescs = target(list(siteDescs),
                    transform = combine(siteDescs, .by = all_sp)),
  comps = target(make_all_bird_comparisons(pairs_df = bird_pairs, siteDescriptions = allDescs),
                 transform = map(allDescs)),
  allCompsSim = target(bind_rows(comps),
                       transform = combine(comps))
)


combine_comps <- drake::drake_plan(
  allComps = bind_rows(comps, allCompsSim)
)

plan <- bind_rows(
  datasets,
  not_datasets,
  bp,
  pulled_datasets,
  real,
  real_splists,
  sim_splists,
  real_describe,
  sim_describe,
  combine_comps
)



## Set up the cache and config
db <- DBI::dbConnect(RSQLite::SQLite(), here::here("drake-cache-bird-pairs-temporal.sqlite"), synchronous = NULL)
cache <- storr::storr_dbi("datatable", "keystable", db)
cache$del(key = "lock", namespace = "session")


## Run the pipeline
nodename <- Sys.info()["nodename"]
if(grepl("ufhpc", nodename)) {
  print("I know I am on the HiPerGator!")
  library(clustermq)
  options(clustermq.scheduler = "slurm", clustermq.template = "slurm_clustermq.tmpl", clustermq.worker.timeout = 1200)
  ## Run the pipeline parallelized for HiPerGator
  make(plan,
       force = TRUE,
       cache = cache,
       verbose = 1,
       parallelism = "clustermq",
       jobs = 20,
       caching = "main", memory_strategy = "autoclean") # Important for DBI caches!
} else {


  # Run the pipeline on multiple local cores
  system.time(make(plan, cache = cache,  verbose = 1, memory_strategy = "autoclean"))


}
#
# loadd(allComps, cache = cache)
#
# write.csv(allComps, "all_bird_comps_nt.csv", row.names =F)
#
# rm(allComps)
#
# loadd(comps, cache = cache)
# write.csv(comps, "real_bird_comps_nt.csv", row.names = F)
#
# rm(comps)
#
# DBI::dbDisconnect(db)
# rm(cache)
# print("Completed OK")
