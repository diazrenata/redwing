library(dplyr)
#library(rwar)
library(drake)
library(MATSS)
library(BBSsize)


run_hpg = T
#max_caps <- c(75, 150, 225, 300, 375, 450, 528)
# for(i in 1:length(max_caps)) {
source(here::here("hasty_fxns.R"))
#i = 1
datasets <- MATSS::build_bbs_datasets_plan()


working_datasets <- read.csv(here::here("supporting_data","perfect_coverage_1988_2018.csv"))


datasets <- datasets[ which(datasets$target %in% working_datasets$matssname), ]
datasets <- datasets[1:2, ]
#
# datasets <- datasets[ unique(c(1:max_caps[i], which(datasets$target %in% c("bbs_rtrg_224_3", "bbs_rtrg_318_3", "bbs_rtrg_19_7", "bbs_rtrg_116_18", "bbs_rtrg_3_80")))), ]

#datasets <- datasets[ which(datasets$target %in% c("bbs_rtrg_224_3", "bbs_rtrg_318_3", "bbs_rtrg_19_7", "bbs_rtrg_116_18", "bbs_rtrg_3_80")), ]

methods <- drake_plan(
  ssims = target(whole_thing(dataset),
                 transform = map(
                   dataset = !!rlang::syms(datasets$target)
                 )),
  all_sims = target(dplyr::bind_rows(ssims),
                    transform = combine(ssims)),
  glms = target(hasty_models(ssims),
                transform = map(ssims)),
  aics = target(hasty_model_aic(glms),
                transform = map(glms)),
  preds = target(hasty_model_predicted_change(glms),
                 transform = map(glms)),
  all_aics = target(dplyr::bind_rows(aics),
                    transform = combine(aics)),
  all_preds = target(dplyr::bind_rows(preds),
                     transform = combine(preds)),
  cor_comps = target(cor_compare(dataset, ndraws = 10),
                     transform = map(
                       dataset = !!rlang::syms(datasets$target)
                     )),
  all_cor_comps = target(dplyr::bind_rows(cor_comps),
                         transform = combine(cor_comps))
)

all = bind_rows(datasets, methods)


## Set up the cache and config
db <- DBI::dbConnect(RSQLite::SQLite(), here::here("drake_caches", "all_hasty_toy.sqlite"))
cache <- storr::storr_dbi("datatable", "keystable", db)
cache$del(key = "lock", namespace = "session")

## Run the pipeline
nodename <- Sys.info()["nodename"]
# if(grepl("ufhpc", nodename)) {
#    print("I know I am on the HiPerGator!")
#
#   if(run_hpg) {
library(clustermq)
options(clustermq.scheduler = "multicore"#, clustermq.template = "slurm_clustermq.tmpl")
)
## Run the pipeline parallelized for HiPerGator
# system.time(make(all,
#                  force = TRUE,
#                  cache = cache,
#                  verbose = 1,
#                  parallelism = "clustermq",
#                  jobs = 6,
#                  caching = "main",
#                  memory_strategy = "autoclean",
#                  lock_envir = F,
#                  garbage_collection = T))# Important for DBI caches!
#   } else {


#Run the pipeline on multiple local cores
system.time(make(all, cache = cache,  verbose = 1, memory_strategy = "autoclean", lock_envir = F, jobs = 3, parallelism = "clustermq"))

