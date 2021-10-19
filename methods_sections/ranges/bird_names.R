library(dplyr)

spnames <- read.csv(here::here("analysis_walkthroughs", "ranges", "bird_names.csv"))

matssnames <- BBSsize::raw_masses %>%
  select(id, english_common_name, sporder, family, genus, species, subspecies) %>%
  distinct() %>%
  mutate(Scientific = paste0(genus," ", species))

matched <- left_join(matssnames, spnames)

sum(is.na(matched$Sequence))

write.csv(matched, "analysis_walkthroughs/ranges/auto_matched.csv", row.names = F)
#
# h <- spnames %>%
# #filter(FamilyName == "Parulidae") %>%
# #filter(grepl("pin", x = Scientific)) #%>%
#   filter(grepl("Virginia's Warbler",x = CommonName))
#   #filter(grepl("hor", x = Synonyms))
#
# nameinfo <- BBSsize::raw_masses %>%
#   filter(english_common_name == "Blue-winged Warbler")

hand_matched <- read.csv("analysis_walkthroughs/ranges/hand_matched.csv")

hand_matched <- filter(hand_matched, !is.na(hand_match_Sequence)) %>% select(id, english_common_name, sporder, family, genus, species, subspecies, hand_match_Sequence, hand_match_notes, hand_match_confident)

hand_matched <- filter(hand_matched, hand_match_confident == 1) %>% rename(Sequence = hand_match_Sequence)# removes Hoary Redpoll for which there is not an exact match

hand_matched <- left_join(hand_matched, spnames)

all_matched <- matched %>%
  filter(!is.na(Sequence)) %>%
  bind_rows(hand_matched)

write.csv(all_matched, "analysis_walkthroughs/ranges/matched_names_done.csv", row.names = F)

all_matched <- read.csv("analysis_walkthroughs/ranges/matched_names_done.csv")

all_matched_query <- paste0("SISID IN ( ", toString(paste0(all_matched$SISRecID)), ")")
all_matched_query
