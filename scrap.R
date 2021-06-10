
```{r}

h <- BBSsize::hartland

h_start <- h$abundance[1:5, ]
h_start <- data.frame(
  species = colnames(h_start),
  total = colSums(h_start)
)
h_start <- h_start %>%
  mutate(total_abund = sum(total)) %>%
  mutate(rel_abund = total / total_abund) %>%
  mutate(timechunk = "start")


h_end <- h$abundance[(nrow(h$abundance) -4) :nrow(h$abundance), ]
h_end <- data.frame(
  species = colnames(h_end),
  total = colSums(h_end)
)
h_end <- h_end %>%
  mutate(total_abund = sum(total)) %>%
  mutate(rel_abund = total / total_abund) %>%
  mutate(timechunk = "end")

h_species <- bind_rows(h_start, h_end) %>%
  select(species, rel_abund, timechunk)

species_size <- BBSsize::sd_table

h_species_size <- left_join(distinct(select(h_species, species)), select(species_size, id, mean_mass), by = c("species" = "id"))

h_species_size <- h_species_size %>%
  arrange(mean_mass) %>%
  mutate(sizerank =  row_number()) %>%
  mutate(ranked_species = factor(sizerank, labels = species))

h_species <- left_join(h_species, h_species_size)
#
# ggplot(h_species, aes(ranked_species, rel_abund, color = timechunk)) +
#   geom_col(data = filter(h_species, timechunk == "start"), alpha = 0) +
#   geom_col(data = filter(h_species, timechunk == "end"), alpha = 0) +
#   scale_color_viridis_d(end = .8)
#
# ggplot(h_species, aes((mean_mass), y = rel_abund, group = ranked_species, color = timechunk)) +
#  geom_col(data = filter(h_species, timechunk == "start"), alpha = 0) +
#  geom_col(data = filter(h_species, timechunk == "end"), alpha = 0) +
#   scale_color_viridis_d(end = .8) +
#   theme(legend.position = "none", panel.grid = element_blank()) +
#   scale_x_log10()
#

h_species_wide <- h_species %>%
  select(ranked_species, timechunk, rel_abund, mean_mass) %>%
  tidyr::pivot_wider(id_cols = c("ranked_species", "mean_mass"), names_from = timechunk, values_from = rel_abund) %>%
  mutate(rel_change = end - start)

ggplot(h_species_wide, aes(mean_mass, rel_change, group = ranked_species, color = rel_change > 0)) +
  geom_errorbar(aes(ymin = 0, ymax = rel_change)) +
  scale_x_log10()+  theme(legend.position = "none")

```
