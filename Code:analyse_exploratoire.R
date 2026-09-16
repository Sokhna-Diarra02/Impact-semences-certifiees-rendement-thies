# ============================================================
# ANALYSE EXPLORATOIRE DU RENDEMENT AGRICOLE
# Projet : Semences certifiées et rendement agricole
# Région de Thiès - Sénégal
# ============================================================

# Packages
library(dplyr)
library(ggplot2)
library(scales)

# ------------------------------------------------------------
# 1. Rendement moyen par type de semences
# ------------------------------------------------------------

rendement_semences <- df %>%
  group_by(Annee, Semence_certifiee) %>%
  summarise(
    rendement_moyen = mean(Rendement, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(
  rendement_semences,
  aes(
    x = Annee,
    y = rendement_moyen,
    color = Semence_certifiee,
    group = Semence_certifiee
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  labs(
    title = "Rendement moyen par type de semences",
    x = "Campagne agricole",
    y = "Rendement moyen (t/ha)",
    color = "Semence certifiée"
  ) +
  theme_minimal()

# ------------------------------------------------------------
# 2. Taux d'adoption des semences certifiées par espèce
# ------------------------------------------------------------

df_pct <- df %>%
  count(Espece, Semence_certifiee) %>%
  group_by(Espece) %>%
  mutate(pourcentage = n / sum(n))

ggplot(df_pct, aes(x = Espece, y = pourcentage, fill = Semence_certifiee)) +
  geom_col(position = position_dodge(width = 0.8)) +
  
  geom_text(
    aes(label = percent(pourcentage, accuracy = 1)),
    position = position_dodge(width = 0.8),
    vjust = -0.3,
    size = 3.5
  ) + scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1.05)    ) +
  
  scale_fill_manual(
    values = c(
      "oui" = "black",
      "Non" = "#D55E00"
    )
  ) +
  labs(
    title = "Taux d'adoption des semence certifié par espéce",
    x = "Espèces cultivées",
    y = "Pourcentage (%)",
    fill = "semence certifiée"
  ) +
  theme_minimal()

# ------------------------------------------------------------
# 3. Variété d'arachide adapté a chaque département
# ------------------------------------------------------------

ggplot(arachide_data,
       aes(x = Variétés,
           y = Rendement,
           fill = Variétés)) +

  geom_boxplot(show.legend = TRUE) +

  facet_wrap(~ Départements) +

  labs(
    title = "rendements  d’arachide par rapport aux variétés culivées dans chaque département",
    x = "Variété cultivée",
    y = "Rendement (t/ha)",
    fill = "Variété"
  ) +

  coord_flip() +   

  theme_minimal() +

  theme(
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 9),
    strip.text = element_text(face = "bold"),
    legend.position = "right"
  )

# ------------------------------------------------------------
#4. comparaison de rendement avec Kruskal test
# ------------------------------------------------------------

comparaison <- list(
  c("Arachide","Maïs"),
  c("Arachide", "Mil"),
  c("Arachide","NIEBE"),
  c("Arachide", "SORGHO")
)



ggplot(df, aes(x = Espece, y = Rendement, fill = Espece)) +
  geom_boxplot() +
  stat_compare_means(method = "kruskal.test") + 
  stat_compare_means(comparison = comparaison, 
                     method = "wilcox.test",
                     exact = FALSE,
                     label = "p.signif")

# ------------------------------------------------------------

# ------------------------------------------------------------


