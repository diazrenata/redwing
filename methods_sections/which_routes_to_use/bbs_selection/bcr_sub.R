#' RMD 10/19/21
#' This is  the original script I used to subsample to a max of 10 routes per BCR.

library(dplyr)


sites <- read.csv(here::here("working_routes.csv"))

set.seed(1989)

bcrs <- sites %>%
  group_by(location.bcr) %>%
  summarize(nroutes = dplyr::n()) %>%
  ungroup()

chosen_sites <- list()

for(i in 1:nrow(bcrs)) {

  these_sites <- filter(sites, location.bcr == bcrs$location.bcr[i])

  if(bcrs$nroutes[i] > 10) {

    sites_to_use <- sample(unique(these_sites$matssname), size = 10, replace = F)

    use_sites <- filter(these_sites, matssname %in% sites_to_use)

  } else {

    use_sites <- these_sites

  }

  chosen_sites[[i]] <- use_sites

}

chosen_sites <- dplyr::bind_rows(chosen_sites)

write.csv(chosen_sites, "working_routes_max10.csv", row.names = F)
