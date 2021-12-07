Just quoting rwar fxns
================

  - [Sims stuff](#sims-stuff)
      - [Drawing of sims](#drawing-of-sims)
      - [Model fitting](#model-fitting)
      - [Model selection](#model-selection)
      - [QIs](#qis)
      - [Diagnostics](#diagnostics)
  - [Comps](#comps)

# Sims stuff

## Drawing of sims

``` r
print(ssims_wrapper)
```

    ## function(dat, simtype = "actual", initial_isd_seed_gmm = 1989, n_isd_draws = 10, ndraws = 100,  initial_draw_seed = 1989) {
    ## 
    ##   if(simtype == "actual") {
    ##     sims <- make_actual_sims(dat, initial_isd_seed_gmm = initial_isd_seed_gmm, n_isd_draws =n_isd_draws, ndraws = ndraws,initial_draw_seed = initial_draw_seed)
    ##   } else if (simtype == "nsc") {
    ##     sims <- make_nosizechange_sims(dat, initial_isd_seed_gmm = initial_isd_seed_gmm, n_isd_draws = n_isd_draws, ndraws = ndraws, initial_draw_seed = initial_draw_seed)
    ##   } else if (simtype == "nc") {
    ##     sims <- make_nochange_sims(dat, initial_isd_seed_gmm = initial_isd_seed_gmm, n_isd_draws = n_isd_draws, ndraws = ndraws, initial_draw_seed = initial_draw_seed)
    ##   }
    ## 
    ##   ssims <- summarize_sims(sims)
    ##   return(ssims)
    ## }
    ## <bytecode: 0x7fa18a4f2aa0>
    ## <environment: namespace:rwar>

``` r
print(make_actual_sims)
```

    ## function(dat, initial_isd_seed_gmm = 1989, n_isd_draws = 10, ndraws = 100, initial_draw_seed = 1989){
    ##   dat_gmms <- rwar::construct_sampling_gmm(dat, n_isd_draws = n_isd_draws, initial_isd_seed = initial_isd_seed_gmm)
    ##   sims <- rwar::draw_communities_wrapper(dat, sampling_gmms = dat_gmms, ndraws = ndraws, initial_draw_seed = initial_draw_seed)
    ##   sims$simtype = "actual"
    ##   return(sims)
    ## }
    ## <bytecode: 0x7fa18a56d398>
    ## <environment: namespace:rwar>

``` r
print(construct_sampling_gmm)
```

    ## function(ts_comp, n_isd_draws = 10, initial_isd_seed = 1989, begin_years = 1988:1992, end_years = 2014:2018) {
    ## 
    ##   # For debugging
    ##   if(is.null(initial_isd_seed)) {
    ##     set.seed(NULL)
    ##     initial_isd_seed <- sample(1:1000000, 1)
    ##   }
    ## 
    ##   isd_seeds <- initial_isd_seed:(initial_isd_seed + n_isd_draws - 1)
    ## 
    ##   # Draw the ISD for the whole timeseries (draws masses for all individuals ever "seen") repeatedly
    ##   # Use seeds for reproducibility
    ##   # You can use the same set of seeds for different sites, etc.
    ## 
    ##   ts_isds_many <- lapply(isd_seeds, just_isd, ts_comp = ts_comp)
    ##   names(ts_isds_many) <- 1:n_isd_draws
    ## 
    ##   # Combine over repeated draws to smooth out sampling variability
    ##   ts_isds_many <- dplyr::bind_rows(ts_isds_many, .id = "sim")
    ## 
    ##   # Pull "begin" and "end" time periods
    ##   ts_isds_begin <- dplyr::filter(ts_isds_many, year %in% begin_years)
    ##   ts_isds_end <- dplyr::filter(ts_isds_many, year %in% end_years)
    ## 
    ##   # Fitting GMMs can be slightly sensitive to starting seed. There's no reason to be fancy about this, but for testing you want this to be fixed.
    ##   set.seed(1977)
    ## 
    ##   # Fit the GMMS to the repeatedly-sampled ISDs. This will give dataframes with mass and density columns.
    ##   ts_gmm_begin <- add_gmm(ts_isds_begin)
    ##   set.seed(1977)
    ## 
    ##   ts_gmm_end <- add_gmm(ts_isds_end)
    ## 
    ##   set.seed(NULL)
    ## 
    ##   # Add timeperiod columns because you want to be sure about that!
    ##   ts_gmm_begin <- dplyr::mutate(ts_gmm_begin, timeperiod = "begin")
    ##   ts_gmm_end <- dplyr::mutate(ts_gmm_end, timeperiod = "end")
    ## 
    ##   return(list(begin = ts_gmm_begin, end = ts_gmm_end))
    ## 
    ## }
    ## <bytecode: 0x7fa18a5cf018>
    ## <environment: namespace:rwar>

``` r
print(draw_communities_wrapper)
```

    ## function(ts_comp, begin_years = 1988:1992, end_years = 2014:2018, ndraws = 100, initial_draw_seed = 1989, sampling_gmms) {
    ## 
    ##   draw_seeds <- initial_draw_seed:(initial_draw_seed + ndraws - 1)
    ## 
    ## 
    ##   # Run draw communities ndraws times.
    ## 
    ##   drawn_communities <- list()
    ##   for(i in 1:length(draw_seeds)) {
    ##     drawn_communities[[i]] <-  draw_communities(ts_comp = ts_comp, begin_years = begin_years, end_years = end_years, sampling_gmms = sampling_gmms, draw_seed = draw_seeds[i])
    ## 
    ##   }
    ## 
    ##   names(drawn_communities) <- 1:ndraws
    ## 
    ##   dplyr::bind_rows(drawn_communities, .id = "sim_iteration")
    ## 
    ## }
    ## <bytecode: 0x7fa18a63c8a8>
    ## <environment: namespace:rwar>

## Model fitting

``` r
print(fit_stanlm)
```

    ## function(some_sims) {
    ## 
    ##   # Fit a stanlm on total_energy
    ##   te_stanlm_full <- rstanarm::stan_glm(total_energy ~ (timeperiod * source) , data = some_sims, iter =8000, thin = 4)
    ##   te_stanlm_nosource <- rstanarm::stan_glm(total_energy ~ (timeperiod), data = some_sims, iter =8000, thin = 4)
    ##   te_stanlm_notime <- rstanarm::stan_glm(total_energy ~ 1, data = some_sims, iter =8000, thin = 4)
    ## 
    ##   te_stanlms = list(
    ##     te_stanlm_full = te_stanlm_full,
    ##     te_stanlm_nosource = te_stanlm_nosource,
    ##     te_stanlm_notime = te_stanlm_notime
    ##   )
    ## 
    ## 
    ##   # Fit the stanlm on total_biomass
    ##   tb_stanlm_full <- rstanarm::stan_glm(total_biomass ~ (timeperiod * source) , data = some_sims, iter = 8000, thin = 4)
    ##   tb_stanlm_nosource <- rstanarm::stan_glm(total_biomass ~ (timeperiod) , data = some_sims, iter = 8000, thin = 4)
    ##   tb_stanlm_notime <- rstanarm::stan_glm(total_biomass ~ 1 , data = some_sims, iter = 8000, thin = 4)
    ## 
    ## 
    ##   tb_stanlms = list(
    ##     tb_stanlm_full = tb_stanlm_full,
    ##     tb_stanlm_nosource = tb_stanlm_nosource,
    ##     tb_stanlm_notime = tb_stanlm_notime
    ##   )
    ## 
    ##   return(list(
    ##     te_stanlms = te_stanlms,
    ##     tb_stanlms = tb_stanlms,
    ##     matssname =some_sims$matssname[1],
    ##     simtype = some_sims$simtype[1]
    ##   ))
    ## 
    ## }
    ## <bytecode: 0x7fa18b1924e0>
    ## <environment: namespace:rwar>

## Model selection

``` r
print(compare_both_stanarms)
```

    ## function(some_stanlms_fits) {
    ## 
    ##   biomass <- compare_stanarms(stanlms_fits = some_stanlms_fits$tb_stanlms)
    ##   energy <- compare_stanarms(stanlms_fits = some_stanlms_fits$te_stanlms)
    ## 
    ##   both_comparisons <- dplyr::bind_rows(biomass = biomass, energy = energy, .id = "currency") %>%
    ##     dplyr::mutate(matssname = some_stanlms_fits$matssname,
    ##                   simtype = some_stanlms_fits$simtype[1])
    ## 
    ## 
    ##   return(both_comparisons)
    ## 
    ## }
    ## <bytecode: 0x7fa18af93340>
    ## <environment: namespace:rwar>

``` r
print(compare_stanarms)
```

    ## function(stanlms_fits) {
    ## 
    ##   stanlms_loos<- lapply(stanlms_fits, rstanarm::loo, k_threshold= .7)
    ## 
    ##   stanlms_comparison <- rstanarm::loo_compare(stanlms_loos) %>%
    ##     as.data.frame() %>%
    ##     dplyr::mutate(model = row.names(.),
    ##                   rank = dplyr::row_number())
    ## 
    ##   return(stanlms_comparison)
    ## 
    ## }
    ## <bytecode: 0x7fa18aff18f0>
    ## <environment: namespace:rwar>

``` r
print(loo_select)
```

    ## function(some_compares) {
    ## 
    ## 
    ##   winners <- some_compares %>%
    ##     dplyr::mutate(model_complexity = ifelse(grepl("full", model), 3,
    ##                                      ifelse(grepl("source", model), 2, 1))) %>%
    ##     dplyr::arrange(matssname, currency, simtype, rank) %>%
    ##     dplyr::group_by(matssname, currency, simtype) %>%
    ##     # dplyr::mutate(best_elpd = elpd_loo[1],
    ##     #               best_se = se_elpd_loo[1]) %>%
    ##     # dplyr::ungroup() %>%
    ##     # dplyr::mutate(one_se_cutoff = best_elpd - best_se) %>%
    ##     dplyr::mutate(in_one_se = (elpd_diff + se_diff) >= 0) %>%
    ##     dplyr::filter(in_one_se) %>%
    ##     dplyr::group_by(currency) %>%
    ##     dplyr::arrange(model_complexity) %>%
    ##     dplyr::mutate(model_rank = dplyr::row_number()) %>%
    ##     dplyr::ungroup() %>%
    ##     dplyr::filter(model_rank == 1)
    ## 
    ##   return(winners)
    ## }
    ## <bytecode: 0x7fa18b864070>
    ## <environment: namespace:rwar>

## QIs

``` r
print(draw_wrapper)
```

    ## function(winners, fits) {
    ##   draws = rwar::winner_draws(winners, fits)
    ##   draw_qis = rwar::winner_qis(draws)
    ##   draw_qis
    ## }
    ## <bytecode: 0x7fa18b9a8be0>
    ## <environment: namespace:rwar>

``` r
print(winner_draws)
```

    ## function(some_winners, some_models) {
    ## 
    ##   winner_energy_mod <- some_winners %>%
    ##     dplyr::filter(currency == "energy") %>%
    ##     dplyr::select(model) %>%
    ##     as.character()
    ## 
    ##   winner_biomass_mod <- some_winners %>%
    ##     dplyr::filter(currency == "biomass") %>%
    ##     dplyr::select(model) %>%
    ##     as.character()
    ## 
    ##   te_draws <- tidybayes::tidy_draws(some_models$te_stanlms[[winner_energy_mod]]) %>%
    ##     dplyr::mutate(currency = "energy", modtype = winner_energy_mod)
    ## 
    ##   tb_draws <- tidybayes::tidy_draws(some_models$tb_stanlms[[winner_biomass_mod]]) %>%
    ##     dplyr::mutate(currency = "biomass", modtype = winner_biomass_mod)
    ## 
    ##   all_draws <- dplyr::bind_rows(te_draws, tb_draws) %>%
    ##     dplyr::mutate(matssname = some_winners$matssname[1],
    ##                   simtype = some_winners$simtype[1])
    ## 
    ##   return(all_draws)
    ## }
    ## <bytecode: 0x7fa18ba0e0e0>
    ## <environment: namespace:rwar>

``` r
print(winner_qis)
```

    ## function(some_draws) {
    ## 
    ##   some_qis <- some_draws %>%
    ##     dplyr::group_by(currency, modtype, matssname, simtype) %>%
    ##     tidybayes::median_qi(.width = c(.95, .99)) %>%
    ##     dplyr::ungroup()
    ## 
    ## 
    ##   return(some_qis)
    ## }
    ## <bytecode: 0x7fa189f125a8>
    ## <environment: namespace:rwar>

## Diagnostics

``` r
print(extract_diagnostics)
```

    ## function(some_fits) {
    ## 
    ##   te_diagnostics <- list()
    ## 
    ##   for(mod_name in names(some_fits$te_stanlms)) {
    ##     nuts <- brms::nuts_params(some_fits$te_stanlms[[mod_name]])
    ##     rhats <- brms::rhat(some_fits$te_stanlms[[mod_name]]) %>%
    ##       t() %>%
    ##       as.data.frame()
    ##     colnames(rhats) <- paste0("rhat_", colnames(rhats))
    ## 
    ##     neffs <- brms::neff_ratio(some_fits$te_stanlms[[mod_name]]) %>%
    ##       t() %>%
    ##       as.data.frame()
    ##     colnames(neffs) <- paste0("neff_", colnames(neffs))
    ## 
    ##     divergents <- nuts %>%
    ##       dplyr::filter(Parameter == "divergent__") %>%
    ##       dplyr::summarize(divergent_sum = sum(Value)) %>%
    ##       dplyr::ungroup()
    ## 
    ##     te_diagnostics[[mod_name]] <- dplyr::bind_cols(neffs, rhats) %>%
    ##       dplyr::mutate(divergent_sum = divergents$divergent_sum,
    ##                     model = mod_name,
    ##                     currency = "energy")
    ##   }
    ##   tb_diagnostics <- list()
    ## 
    ##   for(mod_name in names(some_fits$tb_stanlms)) {
    ##     nuts <- brms::nuts_params(some_fits$tb_stanlms[[mod_name]])
    ##     rhats <- brms::rhat(some_fits$tb_stanlms[[mod_name]]) %>%
    ##       t() %>%
    ##       as.data.frame()
    ##     colnames(rhats) <- paste0("rhat_", colnames(rhats))
    ## 
    ##     neffs <- brms::neff_ratio(some_fits$tb_stanlms[[mod_name]]) %>%
    ##       t() %>%
    ##       as.data.frame()
    ##     colnames(neffs) <- paste0("neff_", colnames(neffs))
    ## 
    ##     divergents <- nuts %>%
    ##       dplyr::filter(Parameter == "divergent__") %>%
    ##       dplyr::summarize(divergent_sum = sum(Value)) %>%
    ##       dplyr::ungroup()
    ## 
    ##     tb_diagnostics[[mod_name]] <- dplyr::bind_cols(neffs, rhats) %>%
    ##       dplyr::mutate(divergent_sum = divergents$divergent_sum,
    ##                     model = mod_name,
    ##                     currency = "biomass")
    ##   }
    ## 
    ##   all_diagnostics <- dplyr::bind_rows(
    ##     dplyr::bind_rows(te_diagnostics),
    ##     dplyr::bind_rows(tb_diagnostics)) %>%
    ##     dplyr::mutate(matssname = some_fits$matssname,
    ##                   simtype = some_fits$simtype)
    ## 
    ##   return(all_diagnostics)
    ## }
    ## <bytecode: 0x7fa18c00c860>
    ## <environment: namespace:rwar>

# Comps

``` r
print(be_comparison)
```

    ## function(dat, initial_isd_seed = 1989, shuffle_seed = 1989, begin_years = 1988:1992, end_years = 2014:2018, nshuffles = 500, n_isd_draws = 5) {
    ## 
    ##   ks_comp <- ks_comparison(dat, initial_isd_seed, begin_years, end_years, shuffle_seed, nshuffles)
    ##   overlap_comp <- overlap_comparison(dat, initial_isd_seed, n_isd_draws, begin_years, end_years)
    ## 
    ##   sp_comp <- sp_comparison(dat, begin_years, end_years)
    ## 
    ## 
    ##   out <- data.frame(
    ##     matssname = paste0("bbs_rtrg_", dat$metadata$route, "_", dat$metadata$region)
    ##   ) %>%
    ##     dplyr::bind_cols(ks_comp, overlap_comp, sp_comp)
    ## 
    ##   out
    ## }
    ## <bytecode: 0x7fa18bb16a98>
    ## <environment: namespace:rwar>

``` r
print(ks_comparison)
```

    ## function(dat, initial_isd_seed = 1989, begin_years = 1988:1992, end_years = 2014:2018, shuffle_seed= 1989, nshuffles = 500) {
    ## 
    ##   raw_isds <- just_isd(dat, initial_isd_seed)
    ## 
    ##   begin_raw_isd <- raw_isds %>%
    ##     dplyr::filter(year %in% begin_years)
    ##   end_raw_isd <- raw_isds %>%
    ##     dplyr::filter(year %in% end_years)
    ## 
    ##   begin_mean_size <- mean(sqrt(begin_raw_isd$mass))
    ##   end_mean_size <- mean(sqrt(end_raw_isd$mass))
    ## 
    ##   begin_mean_e <- mean(sqrt(estimate_b(begin_raw_isd$mass)))
    ##   end_mean_e <- mean(sqrt(estimate_b(end_raw_isd$mass)))
    ## 
    ##   begin_median_size <- median(begin_raw_isd$mass)
    ##   end_median_size <- median(end_raw_isd$mass)
    ## 
    ##   begin_median_e <- median(estimate_b(begin_raw_isd$mass))
    ##   end_median_e <- median(estimate_b(end_raw_isd$mass))
    ## 
    ##   actual_ks_test = ks.test(begin_raw_isd$mass, end_raw_isd$mass)
    ## 
    ##   begin_nind = nrow(begin_raw_isd)
    ##   end_nind = nrow(end_raw_isd)
    ## 
    ##   all_ind = c(begin_raw_isd$mass, end_raw_isd$mass)
    ## 
    ##   total_nind = sum(begin_nind, end_nind)
    ## 
    ##   shuffles <- list()
    ## 
    ##   set.seed(shuffle_seed)
    ## 
    ##   for(i in 1:nshuffles){
    ## 
    ##     shuffle_begin <- sample.int(total_nind, size = begin_nind, replace = F)
    ## 
    ##     begin_masses <- all_ind[shuffle_begin]
    ##     end_masses <- all_ind[-shuffle_begin]
    ## 
    ##     shuffle_ks <- ks.test(begin_masses, end_masses)
    ## 
    ##     shuffles[[i]] <- data.frame(
    ##       it = i,
    ##       d = shuffle_ks$statistic,
    ##       p = shuffle_ks$p.value
    ##     )
    ##   }
    ## 
    ##   shuffles <- dplyr::bind_rows(shuffles)
    ## 
    ##   ks_results <- data.frame(
    ##     actual_d = actual_ks_test$statistic,
    ##     actual_p = actual_ks_test$p.value,
    ##     d_ses = ses(actual_ks_test$statistic, shuffles$d),
    ##     d_percentile = percentile_score(actual_ks_test$statistic, shuffles$d),
    ##     begin_mean_size = begin_mean_size,
    ##     end_mean_size  = end_mean_size,
    ##     begin_mean_e = begin_mean_e,
    ##     end_mean_e = end_mean_e,
    ##     begin_median_size = begin_median_size,
    ##     end_median_size  = end_median_size,
    ##     begin_median_e = begin_median_e,
    ##     end_median_e = end_median_e
    ##   )
    ## 
    ##   set.seed(NULL)
    ## 
    ## 
    ##   ks_results
    ## }
    ## <bytecode: 0x7fa18c8c7da0>
    ## <environment: namespace:rwar>

``` r
print(overlap_comparison)
```

    ## function(dat, initial_isd_seed = 1989, n_isd_draws = 5, begin_years = 1988:1992, end_years = 2014:2018) {
    ## 
    ##   sampling_gmms <- construct_sampling_gmm(dat, n_isd_draws = n_isd_draws, initial_isd_seed = initial_isd_seed, begin_years = begin_years, end_years = end_years)
    ## 
    ##   isd_overlap <- sampling_gmms$begin %>%
    ##     dplyr::left_join(sampling_gmms$end, by = "mass") %>%
    ##     dplyr::group_by_all() %>%
    ##     dplyr::mutate(mindensity = min(density.x, density.y)) %>%
    ##     dplyr::ungroup() %>%
    ##     dplyr::summarize(overlap = sum(mindensity))
    ## 
    ##   isd_overlap
    ## }
    ## <bytecode: 0x7fa18c15e000>
    ## <environment: namespace:rwar>

``` r
print(sp_comparison)
```

    ## function(dat, begin_years = 1988:1992, end_years = 2014:2018) {
    ## 
    ##   begin_comp <- dat$abundance[ which(dat$covariates$year %in% begin_years), ]
    ##   end_comp <- dat$abundance[ which(dat$covariates$year %in% end_years), ]
    ## 
    ##   begin_totals <- colSums(begin_comp)
    ##   end_totals <- colSums(end_comp)
    ## 
    ##   commatrix <- dplyr::bind_rows(begin_totals, end_totals) %>% as.matrix()
    ## 
    ##   bcd <- vegan::vegdist(commatrix)
    ##   bcd_binary <- vegan::vegdist(commatrix, binary = T)
    ##   jac <- vegan::vegdist(commatrix, "jaccard", FALSE)
    ##   jac_binary <- vegan::vegdist(commatrix, "jaccard", TRUE)
    ## 
    ##   sp_overlap <- dplyr::bind_rows(begin_totals, end_totals) %>%
    ##     dplyr::mutate(timeperiod = c("begin", "end")) %>%
    ##     tidyr::pivot_longer(-timeperiod, names_to = "sp", values_to= "abund") %>%
    ##     dplyr::group_by(timeperiod) %>%
    ##     dplyr::mutate(total_abund = sum(abund)) %>%
    ##     dplyr::ungroup() %>%
    ##     dplyr::mutate(relAbund = abund / total_abund) %>%
    ##     tidyr::pivot_wider(id_cols = sp,
    ##                        names_from = timeperiod,
    ##                        values_from = relAbund) %>%
    ##     dplyr::group_by_all() %>%
    ##     dplyr::mutate(minRelAbund = min(begin, end)) %>%
    ##     dplyr::ungroup() %>%
    ##     dplyr::summarize(sp_overlap = sum(minRelAbund)) %>%
    ##     dplyr::mutate(bcd = bcd[1],
    ##                   bcd_binary = bcd_binary[1],
    ##                   jac = jac[1],
    ##                   jac_binary = jac_binary[1])
    ## 
    ##   sp_overlap
    ## 
    ## }
    ## <bytecode: 0x7fa18bbc2c00>
    ## <environment: namespace:rwar>
