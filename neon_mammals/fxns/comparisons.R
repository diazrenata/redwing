pull_pairs <- function(site.x, site.y, all_rats_counts) {

  return(list(
    counts.x = pull_neon_dat(all_rats_counts, site_to_pull = site.x),
    counts.y = pull_neon_dat(all_rats_counts, site_to_pull = site.y)
  ))

}

describe_site <- function(site, sd_table, all_rats_counts) {

  thisSite <- filter(all_rats_counts, siteID == site)
  max_mamm_size <- max(sd_table$mean_wgt) * 1.5

  thisISD = sim_neon_isd(thisSite, sd_table)
  thisGMM = add_gmm(thisISD, max_size = max_mamm_size)
  counts = all_rats_counts %>%
    select(scientificName) %>%
    distinct() %>%
    left_join(select(thisSite, scientificName, abund)) %>%
    group_by_all() %>%
    mutate(abund = ifelse(is.na(abund), 0, abund)) %>%
    ungroup()

  thisAllRel <- counts %>%
    mutate(totalAbund = sum(abund)) %>%
    mutate(relAbund = abund / totalAbund)  %>%
    select(scientificName, relAbund)

  thisAllCounts <- counts %>%
    tidyr::pivot_wider(names_from = scientificName, values_from = abund)

  thisMeta <- all_rats_counts  %>%
    select(-siteID, -scientificName, -abund, -oldName) %>%
    distinct()

  return(list(
    siteID = site,
    isd = thisISD,
    gmm = thisGMM,
    allCounts = thisAllCounts,
    allRel = thisAllRel,
    metaInfo = thisMeta
  ))

}

compare_neon_pairs <- function(siteList.x, siteList.y) {

  gmm.x = siteList.x$gmm
  gmm.y = siteList.y$gmm

  pair_isds <- left_join(gmm.x, gmm.y, by = "mass")

  ol <- pair_isds %>%
    group_by_all() %>%
    mutate(mindensity = min(density.x, density.y)) %>%
    ungroup()

  isd_overlap <- sum(ol$mindensity)

  rel.x = siteList.x$allRel
  rel.y = siteList.y$allRel

  pair_spp <- left_join(rel.x, rel.y, by = "scientificName")

  sp_ol <- pair_spp %>%
    group_by_all() %>%
    mutate(minRelAbund = min(relAbund.x, relAbund.y)) %>%
    ungroup()

  species_overlap = sum(sp_ol$minRelAbund)

  counts_matrix <- bind_rows(
    siteList.x$allCounts,
    siteList.y$allCounts
  ) %>%
    as.matrix()

  bcd = vegan::vegdist(counts_matrix, "bray")[[1]]

  return(data.frame(
    site.x = siteList.x$siteID,
    site.y = siteList.y$siteID,
    isd_overlap = isd_overlap,
    species_overlap = species_overlap,
    bcd = bcd
  ) %>%
    bind_cols(siteList.x$metaInfo)
  )

}

make_all_comparisons <- function(pairs_df, siteDescriptions) {

  siteIDS <- lapply(siteDescriptions, FUN = function(list) return(list$siteID))
  siteIDS <- as.vector(siteIDS)
  names(siteDescriptions) <- siteIDS

  all_comparisons_real <- list()

  for(i in 1:nrow(pairs_df)) {
    all_comparisons_real[[i]] <- compare_neon_pairs(siteDescriptions[[pairs_df$site.x[i]]], siteDescriptions[[pairs_df$site.y[i]]])
  }

  all_comp_df <- bind_rows(all_comparisons_real) %>%
    left_join(pairs_df)

  return(all_comp_df)
}
