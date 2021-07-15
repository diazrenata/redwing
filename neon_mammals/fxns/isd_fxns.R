
pull_neon_dat <- function(all_rats_counts, site_to_pull) {

  this_site <- filter(all_rats_counts, siteID == site_to_pull)

  return(this_site)
}

sim_neon_isd <- function(site_dat, sd_table) {

  simulated_individuals <- list()

  for(i in 1:nrow(site_dat)) {

    these_pars <- filter(sd_table, scientificName == site_dat$scientificName[i])

    these_individuals <- rnorm(n = site_dat$abund[i], mean = these_pars$mean_wgt, sd = these_pars$sd_to_use)

    while(any(these_individuals < 0)) {

      these_individuals[ which(these_individuals < 0)] <- rnorm(n = sum(these_individuals < 0), mean = these_pars$mean_wgt, sd = these_pars$sd_to_use)

    }

    simulated_individuals[[i]] <- data.frame(
      scientificName = site_dat$scientificName[i],
      mass = these_individuals)

  }


  simulated_individuals <- bind_rows(simulated_individuals) %>%
    bind_cols(distinct(select(site_dat, -abund, -scientificName, -oldName)))

  return(simulated_individuals)

}


