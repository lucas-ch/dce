library(tidyverse)
library(patchwork) # Pour combiner les graphiques
library(corrplot)  # Pour la matrice de corrélation

df <- readRDS("data_dce.rds")
design_dce <- readRDS("design_dce.rds")

# --- 1. Préparation des données ---
# S'assurer que les variables catégorielles sont des facteurs
df_plot <- df %>%
  mutate(
    filiere = factor(filiere, labels = c("Economie", "Management", "AES")),
    etudes = factor(etudes, labels = c("Licence 3", "Master 1", "Master 2")),
    genre = factor(genre, labels = c("Femme", "Homme")),
    tt_fact = factor(tt)
  )

# --- 2. Analyses Démographiques (Validation de votre simulation) ---
# Graphique pour la filière (80% / 10% / 10%)
p1 <- ggplot(df_plot %>% distinct(id, .keep_all = TRUE), aes(x = filiere, fill = filiere)) +
  geom_bar() +
  labs(title = "Répartition par Filière", y = "Nombre de répondants") +
  theme_minimal()

# Graphique pour le niveau d'études (80% au niveau 5)
p2 <- ggplot(df_plot %>% distinct(id, .keep_all = TRUE), aes(x = etudes, fill = etudes)) +
  geom_bar() +
  labs(title = "Répartition par Niveau d'Études", y = "Nombre de répondants") +
  theme_minimal()

# --- 3. Analyse des Choix (Préférences Brutes) ---
# Taux de choix selon le Télétravail
p3 <- df_plot %>%
  group_by(tt) %>%
  summarise(prob = mean(choice_bin)) %>%
  ggplot(aes(x = factor(tt), y = prob, group = 1)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(size = 3, color = "steelblue") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(title = "Effet brut du Télétravail", x = "Jours de TT", y = "% de Choix") +
  theme_minimal()

# Taux de choix selon le Salaire (on crée des bins pour la lisibilité)
p4 <- df_plot %>%
  group_by(salaire) %>%
  summarise(prob = mean(choice_bin)) %>%
  ggplot(aes(x = salaire, y = prob)) +
  geom_col(fill = "#00BFC4") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Effet brut du Salaire", x = "Tranches de Salaire", y = "% de Choix") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# --- 4. Analyse des Interactions (Visualisation) ---
# Interaction Télétravail x Distance
p5 <- df_plot %>%
  mutate(dist_cat = ifelse(distance > median(distance), "Loin", "Proche")) %>%
  group_by(tt, dist_cat) %>%
  summarise(prob = mean(choice_bin)) %>%
  ggplot(aes(x = tt, y = prob, color = dist_cat)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Interaction : TT et Distance", x = "Jours de TT", y = "% de Choix", color = "Distance") +
  theme_minimal()

# --- 5. Matrice de Corrélation (Validation du Design) ---
# On vérifie que les attributs ne sont pas corrélés (Design expérimental)
cor_matrix <- cor(design_dce[, c("tt", "salaire", "distance", "ergo", "flex")])
# Pour afficher : corrplot(cor_matrix, method = "color", type = "upper")

# --- 6. Affichage Combiné ---
(p1 | p2) / (p3 | p4) / p5


library(viridis) # Pour une échelle de couleurs lisible

# 1. Calculer le pourcentage de choix par combinaison Salaire/TT
heatmap_data <- df %>%
  group_by(salaire, tt) %>%
  summarise(
    total_presente = n(),                      # Nombre de fois que ce duo est apparu
    total_choisi = sum(choice_bin),            # Nombre de fois qu'il a été choisi
    pct_choisi = total_choisi / total_presente, # Taux de succès
    .groups = 'drop'
  )

# 2. Créer le graphique
ggplot(heatmap_data, aes(x = factor(tt), y = factor(salaire), fill = pct_choisi)) +
  geom_tile(color = "white", size = 0.5) + # Crée les carrés
  geom_text(aes(label = scales::percent(pct_choisi, accuracy = 1)), 
            color = "white", fontface = "bold") + # Ajoute le texte du %
  scale_fill_viridis(option = "D", labels = scales::percent) +
  labs(
    title = "Attractivité des combinaisons Salaire x Télétravail",
    subtitle = "Pourcentage de choix moyen pour chaque combinaison d'attributs",
    x = "Jours de Télétravail (tt)",
    y = "Salaire",
    fill = "Taux de choix"
  ) +
  theme_minimal()


# 1. Calculer le taux de choix de l'Alternative 1 par set
check_dominant <- df %>%
  filter(alt == 1) %>% # On ne garde qu'une ligne par set pour calculer le taux
  group_by(set) %>%
  summarise(
    pct_alt1 = mean(choice_bin),
    total = n()
  ) %>%
  mutate(is_obvious = pct_alt1 > 0.85 | pct_alt1 < 0.15) # Marquer les questions extrêmes

# 2. Plot
ggplot(check_dominant, aes(x = factor(set), y = pct_alt1, fill = is_obvious)) +
  geom_col() +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "red", alpha = 0.5) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  scale_fill_manual(values = c("steelblue", "#FF6666"), 
                    labels = c("Équilibrée", "Potentiellement évidente")) +
  labs(
    title = "Détection des questions 'Évidentes'",
    subtitle = "Pourcentage de choix pour l'Alternative 1 par Set",
    x = "Numéro du Set (Question)",
    y = "% Choix Alternative 1",
    fill = "Statut"
  ) +
  theme_minimal()
