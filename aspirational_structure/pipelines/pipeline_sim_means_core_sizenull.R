library(dplyr)
#library(rwar)
library(drake)
library(MATSS)
library(BBSsize)
library(brms)
library(tidybayes)

run_hpg = T
max_caps <- c(125, 350, 528)
for(i in 1:length(max_caps)) {

datasets <- MATSS::build_bbs_datasets_plan()


working_datasets <- read.csv(here::here("aspirational_structure", "supporting_data", "working_routes.csv"))


datasets <- datasets[ which(datasets$target %in% working_datasets$matssname), ]




datasets <- datasets[ unique(c(1:max_caps[i], which(datasets$target %in% c("bbs_rtrg_224_3", "bbs_rtrg_318_3", "bbs_rtrg_19_7", "bbs_rtrg_116_18", "bbs_rtrg_3_80")))), ]
#
#datasets <- datasets[ which(datasets$target %in% c("bbs_rtrg_224_3", "bbs_rtrg_318_3", "bbs_rtrg_19_7", "bbs_rtrg_116_18", "bbs_rtrg_3_80")), ]
#datasets <- datasets[ which(datasets$target %in% c( "bbs_rtrg_116_18", "bbs_rtrg_224_3")), ]
# datasets <- datasets[ which(datasets$target %in% c("bbs_rtrg_116_18")), ]

null_seeds <- c(1989)
core_transient_sizenull <- function(dat, core_only = F, null_seed = 1989){


  sp_occurrences <- dat$abundance > 0

  sp_presences <- data.frame(
    sp = colnames(sp_occurrences),
    noccurrences = colSums(sp_occurrences),
    nyears = nrow(sp_occurrences)
  ) %>%
    dplyr::mutate(prop_occurrences = noccurrences / nyears) %>%
    dplyr::mutate(is_transient = prop_occurrences < 1/3,
                  is_core = prop_occurrences > 2/3) # following Coyle et al.

  not_transient <- sp_presences %>%
    dplyr::filter(!is_transient)

  core <- sp_presences %>%
    dplyr::filter(is_core)


  if(core_only) {

    sp_to_keep <- core$sp

  } else {

    sp_to_keep <- not_transient$sp
  }

  dat_out <- dat

  dat_out$abundance <- dat_out$abundance[ , sp_to_keep]

  set.seed(null_seed)
  colnames(dat_out$abundance) <- sample(colnames(dat$abundance), size = length(sp_to_keep), replace = F)
  set.seed(NULL)

  dat_out$metadata$species_table <- dat_out$metadata$species_table %>%
    dplyr::filter(id %in% sp_to_keep)

  dat_out
}

core_datasets <- drake_plan(
  coredat = target(core_transient_sizenull(dataset, core_only =T, null_seed= null_seed),
                   transform = cross(
                     dataset = !!rlang::syms(datasets$target),
                     null_seed = !!null_seeds
                   )
))

methods <- drake_plan(
  ssims = target(rwar::ssims_wrapper(dataset, simtype, n_isd_draws = 5, ndraws = 5),
                 transform = cross(
                   dataset = !!rlang::syms(core_datasets$target),
                   simtype = c("actual")#, "nc", "nsc")
                 ) ),
  as = target(dplyr::combine(ssims),
             transform = combine(ssims)),
  all_sims = target(dplyr::bind_rows(as)),
  fits = target(rwar::fit_stanlm(ssims),
                transform = map(ssims)),
  fits_compare = target(rwar::compare_both_stanarms(fits),
                        transform = map(fits)),
  af = target(dplyr::combine(fits_compare),
             transform = combine(fits_compare)),
  all_comparisons = target(dplyr::bind_rows(af, .id = "drakename")),
  winners = target(rwar::loo_select(fits_compare),
                   transform = map(fits_compare)),
  aw = target(dplyr::combine(winners),
              transform = combine(winners)),
  all_winners  = target(dplyr::bind_rows(aw)),
  # diag = target(rwar::extract_diagnostics(fits),
  #               transform = map(fits)),
 # adg = target(dplyr::combine(diag),
 #              transform = combine(diag)),
 # all_diagnostics = target(dplyr::bind_rows(adg)),
 # draws = target(rwar::winner_draws(winners, fits),
 #                transform = map(winners, fits)),
  #ad = target(dplyr::combine(draws),
   #           transform = combine(draws)),
 # all_draws = target(dplyr::bind_rows(ad)),
 qis = target(rwar::draw_wrapper(winners, fits),
               transform = combine(winners, fits, .by = fits)),
  aq = target(dplyr::combine(qis),
             transform = combine(qis)),
  all_qis = target(dplyr::bind_rows(aq))
)

all = bind_rows(datasets,core_datasets,  methods)


## Set up the cache and config
db <- DBI::dbConnect(RSQLite::SQLite(), here::here("aspirational_structure", "drake_caches", "coresizenull.sqlite"))
cache <- storr::storr_dbi("datatable", "keystable", db)
cache$del(key = "lock", namespace = "session")

## Run the pipeline
nodename <- Sys.info()["nodename"]
# if(grepl("ufhpc", nodename)) {
#    print("I know I am on the HiPerGator!")

if(run_hpg) {
  library(clustermq)
  options(clustermq.scheduler = "multicore"#, clustermq.template = "slurm_clustermq.tmpl")
  )
  ## Run the pipeline parallelized for HiPerGator
  system.time(make(all,
                   force = TRUE,
                   cache = cache,
                   verbose = 1,
                   parallelism = "clustermq",
                   jobs = 12,
                   caching = "main",
                   memory_strategy = "autoclean",
                   lock_envir = F,
                   garbage_collection = T))# Important for DBI caches!
} else {


  # Run the pipeline on multiple local cores
  system.time(make(all, cache = cache,  verbose = 1, memory_strategy = "autoclean", lock_envir = F))


}
#
 loadd(all_sims, all_winners, all_qis,cache = cache)#,  all_qis, all_diagnostics, cache = cache)
 save(all_sims, all_winners,all_qis, file = "portable_results_coresizenull.Rds") # all_qis, all_diagnostics, file = "portable_results_coresizenull.Rds")
}

DBI::dbDisconnect(db)
rm(cache)
print("Completed OK")

