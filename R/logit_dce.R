library(tidyverse)
library(survival)

# 1. Chargement et Modèle
df <- readRDS("data_dce.rds")

clogout <- clogit(choice_bin ~ tt + salaire + distance + ergo + 
                    strata(id, round), 
                  data = df)

# 2. Extraction propre des résultats
# summary(model)$coefficients renvoie une matrice : 
# Col 1 = coef, Col 3 = se(coef), Col 5 = p-value
res_mat <- summary(clogout)$coefficients

df_res <- data.frame(
  variable = rownames(res_mat),
  beta     = res_mat[,1],
  se       = res_mat[,3],
  p_val    = res_mat[,5]
)

# 3. Calcul du Willingness To Pay (WTP)
# Formule : WTP_x = -(beta_x / beta_salaire)
beta_sal <- df_res$beta[df_res$variable == "salaire"]

df_wtp <- df_res %>%
  filter(variable != "salaire") %>%
  mutate(
    wtp = (beta / beta_sal),
    # Calcul des intervalles de confiance pour le WTP (Méthode Delta simplifiée)
    wtp_low = ((beta - 1.96 * se) / beta_sal),
    wtp_high = ((beta + 1.96 * se) / beta_sal)
  )
