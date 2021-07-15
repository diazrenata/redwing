
make_counts <- function(all_neon) {

  all_neon_counts <- all_neon %>%
    group_by(siteID, scientificName) %>%
    summarize(abund = dplyr::n()) %>%
    ungroup() %>%
    mutate(sim = -99,
           oldName = scientificName)

  return(all_neon_counts)
}


make_pairs <- function(all_neon) {
neon_sites <- select(all_neon, siteID, decimalLatitude, decimalLongitude) %>%
  group_by(siteID) %>%
  mutate(locnumber = dplyr::row_number()) %>%
  ungroup() %>%
  filter(locnumber == 1) %>%
  select(-locnumber)

neon_site_pairs <- as.data.frame(
  expand.grid(
    site1 = neon_sites$siteID,
    site2 = neon_sites$siteID
  )
) %>%
  mutate(site1 = as.character(site1),
         site2 = as.character(site2)) %>%
  filter(site1 != site2) %>%
  group_by_all() %>%
  mutate(sitealpha1 = min(site1, site2),
         sitealpha2 = max(site1, site2)) %>%
  ungroup() %>%
  select(sitealpha1, sitealpha2) %>%
  distinct() %>%
  rename(site.x = sitealpha1,
         site.y = sitealpha2) %>%
  left_join(neon_sites, by = c("site.x" = "siteID")) %>%
  left_join(neon_sites, by = c("site.y" = "siteID")) %>%
  group_by_all() %>%
  mutate(haver = geosphere::distHaversine(p1 = c(decimalLongitude.x, decimalLatitude.x), p2 = c(decimalLongitude.y, decimalLatitude.y)))  %>%
  ungroup()

return(neon_site_pairs)

}
