#' Get an ISD
#'
#' Tiny wrapper for `BBSsize::simulate_isd_ts` to get just the ISD component of that output.
#'
#' @param ts_comp matss-shaped
#' @param isd_seed only provide for testing
#'
#' @return dataframe of isd
#' @export
#'
#' @importFrom BBSsize simulate_isd_ts
just_isd <- function(ts_comp, isd_seed = NULL) {
  library(BBSsize)
  BBSsize::simulate_isd_ts(ts_comp, isd_seed = isd_seed)$isd
}

#' Construct GMMs for sampling
#'
#' Constructs well-resolved smooths for the begin and end ISDs to use for resampling. Repeatedly draws ISDs by sampling from species' intraspecific body size distributions and combines these repeated samples in to one large sample. Then fits a GMM to this sample to get a very smooth version of the GMM-ified ISD. This allows us to resample ISDs with noninteger ratios of species abundances in a consistent way.
#'
#' @param ts_comp matss-shaped dataset
#' @param n_isd_draws how many versions of the ISD to use to construct the density function. There isn't too much variation between them, defaults to 5. Adding more increases time nonlinear due to the GMM, do not recommend much more than 10.
#' @param initial_isd_seed don't recommend providing unless testing
#' @param begin_years defaults 1988-1992
#' @param end_years defaults 2014-2018
#'
#' @return list of two gmm densities as dataframes with columns mass, density, timeperiod
#' @importFrom dplyr filter bind_rows mutate
#' @importFrom rwar add_gmm
construct_sampling_gmm <- function(ts_comp, n_isd_draws = 5, initial_isd_seed = NULL, begin_years = 1988:1992, end_years = 2014:2018) {

  # For debugging
  if(is.null(initial_isd_seed)) {
    set.seed(NULL)
    initial_isd_seed <- sample(1:1000000, 1)
  }

  set.seed(initial_isd_seed)

  isd_seeds <- sample.int(10000000, size = n_isd_draws)

  # Draw the ISD for the whole timeseries (draws masses for all individuals ever "seen") repeatedly.
  ts_isds_many <- lapply(isd_seeds, just_isd, ts_comp = ts_comp)
  names(ts_isds_many) <- 1:n_isd_draws

  # Combine over repeated draws to smooth out sampling variability
  ts_isds_many <- dplyr::bind_rows(ts_isds_many, .id = "sim")

  # Pull "begin" and "end" time periods
  ts_isds_begin <- dplyr::filter(ts_isds_many, year %in% begin_years)
  ts_isds_end <- dplyr::filter(ts_isds_many, year %in% end_years)

  # Fitting GMMs can be slightly sensitive to starting seed. There's no reason to be fancy about this, but for testing you want this to be fixed.
  set.seed(1977)

  # Fit the GMMS to the repeatedly-sampled ISDs. This will give dataframes with mass and density columns.
  ts_gmm_begin <- rwar::add_gmm(ts_isds_begin)
  ts_gmm_end <- rwar::add_gmm(ts_isds_end)

  set.seed(NULL)

  # Add timeperiod columns because you want to be sure about that!
  ts_gmm_begin <- dplyr::mutate(ts_gmm_begin, timeperiod = "begin")
  ts_gmm_end <- dplyr::mutate(ts_gmm_end, timeperiod = "end")

  return(list(begin = ts_gmm_begin, end = ts_gmm_end))

}


#' Draw individuals from a GMM
#'
#' Draw individual body masses from a GMM density function for the ISD
#'
#' @param nind how many to draw
#' @param sampling_gmm a dataframe with mass (on log scale) and density columns.
#' @param draw_seed don't recommmend providing
#'
#' @return dataframe of drawn (log) body masses, and metainfo for whether the sampling gmm is from begin or end and the seed used when drawing
#' @export
#'
draw_individuals <- function(nind, sampling_gmm, draw_seed = NULL) {

  # The whole point of having draw_seed is for reproducibility at a kind of post-mortem level. I don't ever want to constrain this sampling process to come out the same way every time when running sims (e.g. to have the first 10 draws always be the same), but I do want to be able to set this for testing & debugging purposes.
  if(is.null(draw_seed)) {
    set.seed(NULL)
    draw_seed <- sample(1:1000000, 1)
  }

  set.seed(draw_seed)
  # The actual sampling
  drawn_individuals <- data.frame(
    logmass = sample(sampling_gmm$mass, nind, T, sampling_gmm$density),
    isd_timeperiod = sampling_gmm$timeperiod[1],
    sampling_seed = draw_seed
  )

  set.seed(NULL)
  drawn_individuals
}

#' Add drawn individuals
#'
#' Adds individuals drawn from a sampling GMM to an ISD-shaped dataframe.
#'
#' @param timeperiod_isd an ISD-shaped dataframe. what matters is the number of rows and that it have mass and id columns.
#' @param sampling_gmm begin or end, just one
#' @param draw_seed Don't recommend providing one.
#'
#' @return dataframe with body mass (and energy use) for those individuals drawn from the sampling_gmm provided
#' @export
#'
#' @importFrom dplyr seelct bind_cols mutate
#' @importFrom BBSsize estimate_b
add_drawn_individuals <- function(timeperiod_isd, sampling_gmm, draw_seed = NULL) {

  # Draw new body masses for the individuals in timeperiod_isd using the sampling gmm provided

  ntodraw = nrow(timeperiod_isd)

  drawn <- draw_individuals(ntodraw, sampling_gmm, draw_seed)

# Swap out the masses in timeperiod_isd. Because the gmm is on log masses, before we get energy use and biomass we have to convert back to the raw values.
  timeperiod_isd <- timeperiod_isd %>%
    dplyr::select(-mass, -id) %>%
    dplyr::bind_cols(drawn) %>%
    dplyr::mutate(mass = exp(logmass)) %>%
    dplyr::mutate(energy = BBSsize::estimate_b(mass))

  timeperiod_isd
}

#' Draw communities
#'
#' High-level fxn to get yearly estimates for the state variables under the scenarios of 1) the ISD is conserved begin-end and 2) the ISD changes begin-end. A way to partition change in biomass or energy use expected given changes in abundance and sampling variation, and changes that must be partially driven by changes in the size shift.
#'
#' @param ts_comp matss-style dataset
#' @param begin_years default 1988:1992.
#' @param end_years default 2014:2018.
#' @param draw_seed don't provide unless you are doing something specific
#' @param sampling_gmms do provide for speed! result of construct_sampling_gmms
#' @param initial_isd_seed don't provide unless you have a specific reason to
#' @param raw_isd_seed don't provide unless you have a reason to
#'
#' @return
#' @export dataframe of simulated individuals
#'
#' @importFrom BBSsize simulate_isd_ts
#' @importFrom dplyr filter mutate bind_rows group_by summarize ungroup n bind_cols
draw_communities <- function(ts_comp, begin_years = 1988:1992, end_years = 2014:2018, draw_seed = NULL, sampling_gmms = NULL, initial_isd_seed = NULL, raw_isd_seed = NULL) {

  # If GMM density smooths to sample from are not provided, get them
  # When running at scale, you want to provide them: 1) to ensure that all sims are being drawn from the same sampling GMMS, 2) for speed, it takes a long time to fit a GMM to a lot of ISD draws.

  if(is.null(sampling_gmms)) {

    sampling_gmms <- construct_sampling_gmm(ts_comp, begin_years = begin_years, end_years = end_years, initial_isd_seed = NULL)

  }

  # This doesn't actually matter, I am using the raw isds to get the correct shape data frame. I am keeping it this way, even though it's inefficient, because in the back of my mind I think I might want to pull out raw estimates for biomass and energy use (i.e. drawing individuals directly from normal distributions for each year, rather than drawing from the ISD combining over all years). It'll be a tiny change but I'm not 100% sure what I want to do with that info yet so holding off.

  if(is.null(raw_isd_seed)) {
    set.seed(NULL)
    raw_isd_seed <- sample(1:1000000, 1)
  }

  # Here I am sampling ISDs to get dfs of the correct shape to then sample new body masses from different density fxns.

  raw_isd <- BBSsize::simulate_isd_ts(ts_comp, isd_seed = raw_isd_seed)$isd

  begin_isd <- dplyr::filter(raw_isd, year %in% begin_years) %>%
    dplyr::mutate(timeperiod = "begin")
  end_isd <- dplyr::filter(raw_isd, year %in% end_years) %>%
    dplyr::mutate(timeperiod = "end")


  # Draw individuals for each time period from the MATCHING density functions
  # This will destroy interannual, intratimeperiod variation in the size structure, which we're OK with (the point of using 5-year intervals is to smooth out species accumulation)
  # I do not think it is a good idea to provide draw_seed, that will constrain things to come out the same in weird ways at this scale.
  begin_individuals <- add_drawn_individuals(begin_isd, sampling_gmms$begin, draw_seed = draw_seed)

  end_individuals <- add_drawn_individuals(end_isd, sampling_gmms$end, draw_seed = draw_seed)

  actual_individuals <- dplyr::bind_rows(begin_individuals, end_individuals) %>%
    dplyr::mutate(source = "actual")


  # Now draw individuals for each time period with scrambled ISDs. Specifically, draw for the beginning from the beginning ISD. But then also draw the end from the beginning ISD. This gives an "end" ISD pretending that the ISD didn't change from the beginning.
  begin_individuals_sim <- add_drawn_individuals(begin_isd, sampling_gmms$begin, draw_seed = draw_seed)

  end_individuals_sim <- add_drawn_individuals(end_isd, sampling_gmms$begin, draw_seed = draw_seed)


  sim_individuals <- dplyr::bind_rows(begin_individuals_sim, end_individuals_sim) %>%
    dplyr::mutate(source = "sim")


  # Go ahead and pull the raw state variable estimates too..

  raw_individuals <- dplyr::bind_rows(begin_isd, end_isd) %>%
    dplyr::mutate(energy = BBSsize::estimate_b(mass),
                  source = "raw",
                  isd_timeperiod = "raw",
                  sampling_seed = NA)


  all_individuals <- dplyr::bind_rows(actual_individuals, sim_individuals, raw_individuals)


  # Summarize individuals to get toal abundance, biomass, and energy use per year for each sim scenario.
  # And add route-level identifying info.
  all_svs <- all_individuals %>%
    dplyr::group_by(year, timeperiod, isd_timeperiod, sampling_seed, isd_seed, source) %>%
    dplyr::summarize(total_abundance = dplyr::n(),
                     total_biomass = sum(mass),
                     total_energy = sum(energy)) %>%
    dplyr::ungroup() %>%
    dplyr::bind_cols(as.data.frame(ts_comp$metadata$location)) %>%
    dplyr::mutate(matssname = paste0("bbs_rtrg_", route, "_", statenum))

  all_svs
}

#' Wrapper for draw_communities
#'
#' To facilitate pipelines. Just runs draw_communities `ndraws` times and tacks on a column keeping track of which sim.
#'
#' @param ts_comp matss-style dataset
#' @param begin_years default 1988:1992.
#' @param end_years default 2014:2018.
#' @param ndraws default 100
#' @param draw_seed don't provide unless you are doing something specific
#' @param sampling_gmms do provide for speed! result of construct_sampling_gmms
#' @param initial_isd_seed don't provide unless you have a specific reason to
#' @param raw_isd_seed don't provide unless you have a reason to
#'
#' @return dataframe of sims
#' @export
#' @importFrom dplyr bind_rows
draw_communities_wrapper <- function(ts_comp, begin_years = 1988:1992, end_years = 2014:2018, ndraws = 100, draw_seed = NULL, sampling_gmms = NULL, initial_isd_seed = NULL, raw_isd_seed = NULL) {

  # Run draw communities ndraws times.
  drawn_communities <- replicate(ndraws, draw_communities(ts_comp, begin_years = begin_years, end_years = end_years, draw_seed = draw_seed, sampling_gmms = sampling_gmms, initial_isd_seed = initial_isd_seed, raw_isd_seed = raw_isd_seed), simplify = F)

  names(drawn_communities) <- 1:ndraws

  dplyr::bind_rows(drawn_communities, .id = "sim_iteration")

}
