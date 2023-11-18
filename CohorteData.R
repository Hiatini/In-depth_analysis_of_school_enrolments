## Hiatini TEKOHUOTETUA


################################  1.QUESTION   ################################

## A partir des données en open data, nous allons préparer plusieurs graphiques afin d’illustrer plusieurs problématiques.
## La première problématique serait de savoir combien d'élèves avons-nous en secondaire en 2020, et ce, par académie ?

###############################################################################

# Charger les bibliothèques
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)

# Charger les données depuis le lien
url <- "https://data.education.gouv.fr/explore/dataset/fr-en-lycee_gt-effectifs-niveau-sexe-lv/download/?format=csv&timezone=Europe/Berlin&use_labels_for_header=true"
donnees <- read.csv2(url, sep =";", stringsAsFactors = TRUE)
View(donnees)

# Sélectionner les colonnes pertinentes
donnees_selectionnees <- donnees %>%
  select(Académie, Rentrée.scolaire, X2ndes.GT, X2ndes.STHR, X2ndes.TMD, X2ndes.BT)

# Filtrer les données pour l'année 2020
donnees_2020 <- donnees_selectionnees %>%
  filter(Rentrée.scolaire == 2020)

# Calculer les effectifs de chaque académie
effectifs_par_academie <- donnees_2020 %>%
  group_by(Académie) %>%
  summarize(Total_Effectifs = sum(X2ndes.GT, X2ndes.STHR, X2ndes.TMD, X2ndes.BT))

# Ranger le tableau par ordre décroissant des effectifs
effectifs_par_academie <- effectifs_par_academie %>%
  arrange(desc(Total_Effectifs))

# Afficher le tableau résultant
View(effectifs_par_academie)

#################################  RESULTATS   #################################

# Dans notre top 5, nous pouvons observer que Versailles est en première position avec 56 887 élèves en seconde, suivi de Créteil avec 41 697 élèves,
# Lille avec 33 935 élèves, Nantes avec 31 023 élèves et Lyon avec 29 457 élèves. Afin d’avoir un meilleur aperçu, nous allons créer un histogramme illustrant
# notre Top Trois des académies ayant le plus d’élèves en second degré en 2020.

################################################################################

# Prendre les trois premières lignes (top trois)
top_three <- head(effectifs_par_academie, 3)

# Créer le graphique
graphique_top_three <- ggplot(top_three, aes(x = reorder(Académie, Total_Effectifs), y = Total_Effectifs)) +
  geom_text(aes(label = Total_Effectifs), vjust = -0.5, size = 3) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Top 3 des académies avec le plus d'élèves en second degré en 2020",
       x = "Académies",
       y = "Effectifs") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.line = element_line(color = "#2E3B4E"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 14, color = "#2E3B4E"))

# Afficher le graphique
print(graphique_top_three)

#################################  RESULTATS   #################################
# La distinction entre Versailles, Créteil et Lille est claire. Versailles conserve la première place avec le plus grand nombre d'élèves en second degré en 2020.
################################################################################

                                ###############

################################  2.QUESTION   ################################

## Une autre question que nous avons explorée concernait la répartition des élèves de 2nde GT selon la LV1 choisie à la rentrée 2021.

###############################################################################

# Sélectionner les colonnes pertinentes et les renommer
donnees_selectionnees <- donnees %>%
  select(Rentrée.scolaire,
         "allemand" = "X2ndes.GT.LV1.allemand",
         "anglais" = "X2ndes.GT.LV1.anglais",
         "espagnol" = "X2ndes.GT.LV1.espagnol",
         "autres_langues" = "X2ndes.GT.LV1.autres.langues")

# Filtrer les données pour l'année 2021
donnees_2021 <- donnees_selectionnees %>%
  filter(Rentrée.scolaire == 2021)

# Faire pivoter les données
donnees_2021_molles <- donnees_2021 %>%
  pivot_longer(
    cols = c(allemand, anglais, espagnol, autres_langues),
    names_to = "LV1",
    values_to = "Effectif"
  )

# Regrouper les lignes correspondant à la même langue
donnees_2021_molles <- donnees_2021_molles %>%
  group_by(LV1) %>%
  summarize(Effectif = sum(Effectif)) %>%
  arrange(desc(Effectif))

View(donnees_2021_molles)

#################################  RESULTATS   #################################

# Selon nos résultats, l'anglais est la langue la plus couramment choisie comme LV1, avec plus de 550 000 élèves en 2nde Générale et Technologique.
# En deuxième position, plus de 11 000 élèves en 2nde étudient l'allemand, tandis que 4 499 élèves optent pour l'espagnol. Enfin, en dernière position,
# nous avons plus de 2 000 élèves étudiant d'autres langues que celles mentionnées.

################################################################################

################################  3.QUESTION   ################################

## La dernière question à laquelle nous souhaitons répondre concerne les académies qui ont enregistré une augmentation du nombre d'élèves entre la
## rentrée 2020 et la rentrée 2021. Quelles académies ont connu une augmentation du nombre d'élèves entre ces deux périodes ?

###############################################################################

# Filtrer les données

donnees_2020 <- donnees %>% filter(Rentrée.scolaire == 2020)
donnees_2021 <- donnees %>% filter(Rentrée.scolaire == 2021)

# Calculer les effectifs par académie
effectifs_2020 <- donnees_2020 %>%
  group_by(Académie) %>%
  summarize(Effectifs_2020 = sum(Nombre.d.élèves))

effectifs_2021 <- donnees_2021 %>%
  group_by(Académie) %>%
  summarize(Effectifs_2021 = sum(Nombre.d.élèves))

# Calculer la différence d'effectifs et trier du plus grand au plus petit
difference_effectifs <- inner_join(effectifs_2020, effectifs_2021, by = "Académie") %>%
  mutate(Différence = Effectifs_2021 - Effectifs_2020) %>%
  arrange(desc(Différence))

# Créer le tableau final
tableau_final <- difference_effectifs %>%
  select(Académie, Différence)

# Calculer le pourcentage d'élèves gagnés
tableau_final <- difference_effectifs %>%
  select(Académie, Différence)

# Afficher le nouveau tableau final
View(tableau_final)

# Afin de mettre en avant notre top trois des graphiques, nous allons le mettre
# en avant en utilisant un histogramme. 

# Prendre les trois premières lignes (top trois)
top_three <- head(difference_effectifs, 3)

# Créer le graphique
graphique_top_three <- ggplot(top_three, aes(x = reorder(Académie, Différence), y = Différence)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Top 3 des académies avec la plus grande différence d'effectifs (2020-2021)",
       x = "Académie",
       y = "Différence d'effectifs") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Afficher le graphique
print(graphique_top_three)

#################################  RESULTATS   #################################

# Nous avons, encore une fois, un top trois avec Versailles, Créteil et Lyon qui sont les académies qui accumulent le plus d’élèves de 2020 à 2021.
# Allons plus loin en cherchant à comprendre les écarts observés dans les gains d'effectifs entre chaque académie et les disparités entre les genres.
# Est-ce qu'il y a une prédominance numérique des filles ou des garçons dans ces variations ?

###############################################################################

# Filtrer les données
donnees_2019 <- donnees %>% filter(Rentrée.scolaire == 2019)

# Calculer les effectifs par académie pour 2019-2020
effectifs_2019_2020_filles <- donnees_2019 %>%
  group_by(Académie) %>%
  summarize(Gain_Filles = sum(X1ères.G.filles) + sum(X1ères.STI2D.filles) + sum(Terminales.G.filles) + sum(Terminales.STI2D.filles) + sum(Terminales.STL.filles) + sum(Terminales.STMG.filles) + sum(Terminales.ST2S.filles) + sum(Terminales.STD2A.filles))

effectifs_2019_2020_garcons <- donnees_2019 %>%
  group_by(Académie) %>%
  summarize(Gain_Garcons = sum(X1ères.G.garçons) + sum(X1ères.STI2D.garçons) + sum(Terminales.G.garçons) + sum(Terminales.STI2D.garçons) + sum(Terminales.STL.garçons) + sum(Terminales.STMG.garçons) + sum(Terminales.ST2S.garçons) + sum(Terminales.STD2A.garçons))

# Calculer les effectifs par académie pour 2020-2021
effectifs_2020_2021_filles <- donnees_2020 %>%
  group_by(Académie) %>%
  summarize(Gain_Filles = sum(X1ères.G.filles) + sum(X1ères.STI2D.filles) + sum(Terminales.G.filles) + sum(Terminales.STI2D.filles) + sum(Terminales.STL.filles) + sum(Terminales.STMG.filles) + sum(Terminales.ST2S.filles) + sum(Terminales.STD2A.filles))

effectifs_2020_2021_garcons <- donnees_2020 %>%
  group_by(Académie) %>%
  summarize(Gain_Garcons = sum(X1ères.G.garçons) + sum(X1ères.STI2D.garçons) + sum(Terminales.G.garçons) + sum(Terminales.STI2D.garçons) + sum(Terminales.STL.garçons) + sum(Terminales.STMG.garçons) + sum(Terminales.ST2S.garçons) + sum(Terminales.STD2A.garçons))

# Fusionner les données
donnees_gagnes <- inner_join(effectifs_2019_2020_filles, effectifs_2019_2020_garcons, by = "Académie") %>%
  inner_join(effectifs_2020_2021_filles, by = "Académie") %>%
  inner_join(effectifs_2020_2021_garcons, by = "Académie") %>%
  select(Académie, Gain_Filles.x, Gain_Garcons.x, Gain_Filles.y, Gain_Garcons.y)

# Renommer les colonnes
colnames(donnees_gagnes) <- c("Académie", "≈", "Gain_Garcons_2019_2020", "Gain_Filles_2020_2021", "Gain_Garcons_2020_2021")

# Afficher le tableau final
View(donnees_gagnes)

# Créer le graphique 1
graphique_histogramme1 <- ggplot(donnees_gagnes, aes(x = reorder(Académie, Gain_Filles_2020_2021), y = Gain_Filles_2020_2021, fill = "Filles")) +
  geom_bar(stat = "identity", position = "identity", color = "black") +
  geom_bar(aes(x = reorder(Académie, Gain_Garcons_2020_2021), y = Gain_Garcons_2020_2021, fill = "Garçons"), stat = "identity", position = "identity", color = "black") +
  scale_fill_manual(values = c("Filles" = "skyblue", "Garçons" = "salmon"), name = "Genre") +
  labs(title = "Nombre d'élèves gagnés par académie (2020-2021)",
       x = "Académies",
       y = "Nombre d'élèves gagnés") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Afficher le graphique
print(graphique_histogramme1)

# Créer le graphique 2
graphique_histogramme2 <- ggplot(donnees_gagnes, aes(x = reorder(Académie, Gain_Filles_2019_2020), y = Gain_Filles_2019_2020, fill = "Filles")) +
  geom_bar(stat = "identity", position = "identity", color = "black") +
  geom_bar(aes(x = reorder(Académie, Gain_Garcons_2019_2020), y = Gain_Garcons_2019_2020, fill = "Garçons"), stat = "identity", position = "identity", color = "black") +
  scale_fill_manual(values = c("Filles" = "skyblue", "Garçons" = "salmon"), name = "Genre") +
  labs(title = "Nombre d'élèves gagnés par académie (2019-2020)",
       x = "Académies",
       y = "Nombre d'élèves gagnés") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Afficher le graphique
print(graphique_histogramme2)


#################################  RESULTATS   #################################

# Nous constatons que parmi les trois meilleures académies en termes de recrutement d'élèves pour l'année scolaire 2020-2021 et l'année scolaire
# 2020-2021, il y a une prédominance du nombre de filles par rapport aux garçons.

###############################################################################
