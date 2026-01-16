library(idefix)
library(tidyverse)
library(jsonlite)

# Créer la matrice complète des attributs
lvls <- c(3, 2, 2, 3, 3)
coding <- c("E","D","D","E","E")

con.lvls <- list(
  c(0, 2, 5),               # teletravail
  c(1934, 2028, 2146)          # salaire
)

candidates <- Profiles(lvls = lvls, coding = coding)

# Selectionner un plan d-efficace
nparam <- 8
priors <- matrix(0, nrow = 1, ncol = nparam)

n_sets <- 16
n_alt <- 2
n_blocks <- 2

set.seed(123)
design_out <- Modfed(
  cand.set  = as.matrix(candidates),
  n.sets    = n_sets,
  n.alts    = n_alt,
  n.blocks = n_blocks,
  par.draws = priors,
)

selected_design <- design_out$BestDesign$design

# Rendre lisible le plan
blocks_df <- design_out$BestDesign$Blocks[[1]] %>%
  as.data.frame() %>%
  rownames_to_column(var = "option") %>%
  mutate(block = 1)

df <- as.data.frame(selected_design) %>%
  rownames_to_column("option") %>%
  separate(option, into = c("set", "alt"), sep = "\\.", remove = FALSE) %>%
  mutate(set = as.numeric(gsub("set", "", set)),
         alt = as.numeric(gsub("alt", "", alt))) %>%
  mutate(
    tt = case_when(
      Var11 == 0 & Var12 == 1 ~ 2,
      Var11 == -1 & Var12 == -1 ~ 5,
      Var11 == 1 & Var12 == 0 ~ 0
    ),
    flex = case_when(
      Var22 == 1 ~ 1,
      Var22 == 0 ~ 0
    ),
    ergo = case_when(
      Var32 == 1 ~ 1,
      Var32 == 0 ~ 0
    ),
    distance = case_when(
      Var41 == -1 & Var42 == -1 ~ 42,
      Var41 == 0 & Var42 == 1 ~ 27,
      Var41 == 1 & Var42 == 0 ~ 11
    ),
    salaire = case_when(
      Var51 == -1 & Var52 == -1 ~ 2146,
      Var51 == 0 & Var52 == 1 ~ 2028,
      Var51 == 1 & Var52 == 0 ~ 1934
    )
  ) %>%
  select(option, set, alt, tt, flex, ergo, distance, salaire)

df <- df %>%
  left_join(blocks_df %>% select(option, block), by = "option") %>%
  mutate(block = coalesce(block, 2))

selected_design_list <- df %>%
  group_by(set) %>%
  summarise(
    scenario = list(
      map(seq_len(n()), ~ list(
        set = set[.x],
        alt = alt[.x],
        block = block[.x],
        tt = tt[.x],
        flex = flex[.x],
        ergo = ergo[.x],
        distance = distance[.x],
        salaire = salaire[.x]
      ))
    ),
    .groups = "drop"
  ) %>%
  pull(scenario)

# Enregister le plan pour utilisation dans scripts R et python
write_json(selected_design_list, "design_dce.json", auto_unbox = TRUE, pretty = TRUE)
saveRDS(df, file = "design_dce.rds")
