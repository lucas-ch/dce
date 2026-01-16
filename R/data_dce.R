library(tidyverse)
library(survival)

# Charger les données à analyser et le plan du DCE
data <- read_csv("../data/all_apps_wide.csv")
design_dce <- readRDS("design_dce.rds")

# Formatter les données pour une analyse logit
data_long <- data %>% 
  select(participant.id_in_session, 
         matches("^dce\\.\\d+\\.player\\.(alt|set|block|selected)")) %>%
  pivot_longer(
    cols = -participant.id_in_session,
    names_to = c("round", ".value"),
    names_pattern = "dce\\.(\\d+)\\.player\\.(.*)"
  ) %>%
  mutate(round = as.integer(round)) %>%
  rename(id = participant.id_in_session)

data_full <- data_long %>%
  select(id, round, set, selected) %>%
  distinct()%>%
  left_join(design_dce, by = "set") %>%
  select(id, round, set, alt,selected, tt, flex, ergo, distance, salaire) %>%
  mutate(choice_bin = ifelse(alt == selected, 1, 0))

# Ajouter les infos démographiques
participant_info <- data %>% 
  select(id = participant.id_in_session, 
         age = dce.1.player.age, 
         genre = dce.1.player.genre, 
         etudes = dce.1.player.etudes, 
         filiere = dce.1.player.filiere)

data_full <- data_full %>%
  left_join(participant_info, by = "id")

saveRDS(data_full, file = "data_dce.rds")