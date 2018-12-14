#### Création séries temporelles ####

# Installation et chargement des packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman, data.table, dplyr, surveillance, RMySQL, stringr, ggplot2, dygraphs, xts)

## Données : SAU, DEP: Paris (75), C_AGE : 6 (tous), RS : 40 (Ischémie myocardique)
# Données hebdomadaires

# Récupération des données

# # Si données dans la BDD
# sau_sem_france_dep <- dbReadTable(con, "sau_sem_france_dep")

# Si sous forme RDS (ici le fichier RDS est la table extraite de la BDD)
sau_sem_france_dep <- readRDS(file = "./data/sau_sem_france_dep_bdd.rds")

# Extraction des données
sau_sem_france_dep <- setDT(sau_sem_france_dep)[age_class == 6 & dep == 75 & synd_group == 40]

# Calcul des proportions
sau_sem_france_dep[, proportion_sau := round(10000*nb_visits/total_actes_codes, digits = 2)]

# Création d'un objet sts
sau_sem_france_dep_sts <- sts(observed = sau_sem_france_dep$proportion_sau, # les proportions
                              start = c(2010, 01), # la première semaine de 2010
                              frequency = 52, # 52 car travail en semaine
                              epochAsDate = TRUE, # les valeurs numériques retournées par epoch sont transformées en date
                              epoch = as.numeric(as.Date(sau_sem_france_dep$d, origin = "1970-01-01"))) # transformation des dates en numérique


# Premier graphique
dygraph(data = sau_sem_france_dep_sts,
        main = "Nombre de passages pour une ischémie myocardique (SAU) pour 10 000 passages codés entre janvier 2010 et décembre 2017 sur Paris (75) pour tous âges",
        xlab = "Date", ylab = "Nombre de passages pour 10 000 passages codés") %>% dyRangeSelector()



# Création d'un objet Disprog (nécessaire pour les algorithmes RKI, Bayes et Farrington)

sau_sem_france_dep_disprog <- sts2disProg(sau_sem_france_dep_sts)

# Calcul du nombre de semaines
nbr_sem <- length(sau_sem_france_dep_disprog$observed)

# On indique sur combien d'années on veut créer nos alarmes
# nbr_sem - (52 * nombre_année)
# Ici on calculera sur 3 années
min_sem <-  nbr_sem - (52*3)


##### RKI 3

# Application de l'algorithme RKI3
sau_sem_france_dep_rki3 <- algo.rki3(disProgObj = sau_sem_france_dep_disprog,
                                     control = list(range = c(min_sem: nbr_sem))) # Période que l'on veut étudier

# On remplit la colonne @alarm avec les alarmes calculées par l'algorithme
sau_sem_france_dep_sts@alarm <- as.matrix(c(rep(0, times = min_sem-1), sau_sem_france_dep_rki3$alarm))

# On remplit la colonne @upperbound avec les alarmes calculées par l'algorithme
sau_sem_france_dep_sts@upperbound <- as.matrix(c(rep(0, times = min_sem - 1), sau_sem_france_dep_rki3$upperbound))

# Création de la série temporelle avec les cas observés
serie1 <- data.frame(date = as.Date(sau_sem_france_dep_sts@epoch), obs = sau_sem_france_dep_sts@observed)
serie1 <- xts(serie1$observed1, order.by = serie1$date)

# Créationd de la série temporelle avec les bornes supérieures
serie2 <- data.frame(date = as.Date(sau_sem_france_dep_sts@epoch), upp = sau_sem_france_dep_sts@upperbound)
# On enlève les bornes supérieures égales à 0
serie2 <- serie2[which(serie2$upp != 0),]
serie2 <- xts(serie2$upp, order.by = serie2$date)

# Fusion des deux séries
serie <- cbind(serie1, serie2)
names(serie) <- c("cas_obs", "borne_sup")

# Création du graphique dypgrah avec la borne supérieure de prédiction
dygraph_sau <- dygraph(serie,
                       main = "Nombre de passages pour une ischémie myocardique (SAU) pour 10 000 passages codés entre janvier 2010 et décembre 2017 sur Paris pour tous âges et seuil (méthode RKI3)",
                       xlab = "Date",
                       ylab = "Nombre de passages pour 10 000") %>%
  dySeries("cas_obs", label = "Proportion observée") %>%
  dySeries("borne_sup", label = "Borne supérieure de détection", strokePattern = "dashed") %>%
  dyOptions(drawGrid = FALSE) %>%
  dyRangeSelector()
dygraph_sau

# On rajoute au graphique dygraph les alarmes
for(i in seq_along(sau_sem_france_dep_sts@alarm)){
  
  if(sau_sem_france_dep_sts@alarm[i] == 1){
    dygraph_sau <- dygraph_sau %>% dyAnnotation(x = as.Date(sau_sem_france_dep_sts@epoch[i], origin = "1970-01-01"), text = "A",
                                                tooltip = as.character(sau_sem_france_dep_sts@observed[i]))
  }
}
dygraph_sau

###### Bayes 2

# Application de l'algorithme Bayes2
sau_sem_france_dep_bayes2 <- algo.bayes2(disProgObj = sau_sem_france_dep_disprog,
                                     control = list(range = c(min_sem: nbr_sem))) # Période que l'on veut étudier

# On remplit la colonne @alarm avec les alarmes calculées par l'algorithme
sau_sem_france_dep_sts@alarm <- as.matrix(c(rep(0, times = min_sem-1), sau_sem_france_dep_bayes2$alarm))

# On remplit la colonne @upperbound avec les alarmes calculées par l'algorithme
sau_sem_france_dep_sts@upperbound <- as.matrix(c(rep(0, times = min_sem - 1), sau_sem_france_dep_bayes2$upperbound))

# Création de la série temporelle avec les cas observés
serie1 <- data.frame(date = as.Date(sau_sem_france_dep_sts@epoch), obs = sau_sem_france_dep_sts@observed)
serie1 <- xts(serie1$observed1, order.by = serie1$date)

# Créationd de la série temporelle avec les bornes supérieures
serie2 <- data.frame(date = as.Date(sau_sem_france_dep_sts@epoch), upp = sau_sem_france_dep_sts@upperbound)
# On enlève les bornes supérieures égales à 0
serie2 <- serie2[which(serie2$upp != 0),]
serie2 <- xts(serie2$upp, order.by = serie2$date)

# Fusion des deux séries
serie <- cbind(serie1, serie2)
names(serie) <- c("cas_obs", "borne_sup")

# Création du graphique dypgrah avec la borne supérieure de prédiction
dygraph_sau <- dygraph(serie,
                       main = "Nombre de passages pour une ischémie myocardique (SAU) pour 10 000 passages codés entre janvier 2010 et décembre 2017 sur Paris pour tous âges et seuil (méthode Bayes 2)",
                       xlab = "Date",
                       ylab = "Nombre de passages pour 10 000") %>%
  dySeries("cas_obs", label = "Proportion observée") %>%
  dySeries("borne_sup", label = "Borne supérieure de détection", strokePattern = "dashed") %>%
  dyOptions(drawGrid = FALSE) %>%
  dyRangeSelector()
dygraph_sau

# On rajoute au graphique dygraph les alarmes
for(i in seq_along(sau_sem_france_dep_sts@alarm)){
  
  if(sau_sem_france_dep_sts@alarm[i] == 1){
    dygraph_sau <- dygraph_sau %>% dyAnnotation(x = as.Date(sau_sem_france_dep_sts@epoch[i], origin = "1970-01-01"), text = "A",
                                                tooltip = as.character(sau_sem_france_dep_sts@observed[i]))
  }
}
dygraph_sau


###### Farrington

# Application de l'algorithme Farrington
sau_sem_france_dep_farr <- algo.farrington(disProgObj = sau_sem_france_dep_disprog,
                                             control = list(range = c(min_sem: nbr_sem))) # Période que l'on veut étudier

# On remplit la colonne @alarm avec les alarmes calculées par l'algorithme
sau_sem_france_dep_sts@alarm <- as.matrix(c(rep(0, times = min_sem-1), sau_sem_france_dep_farr$alarm))

# On remplit la colonne @upperbound avec les alarmes calculées par l'algorithme
sau_sem_france_dep_sts@upperbound <- as.matrix(c(rep(0, times = min_sem - 1), sau_sem_france_dep_farr$upperbound))

# Création de la série temporelle avec les cas observés
serie1 <- data.frame(date = as.Date(sau_sem_france_dep_sts@epoch), obs = sau_sem_france_dep_sts@observed)
serie1 <- xts(serie1$observed1, order.by = serie1$date)

# Créationd de la série temporelle avec les bornes supérieures
serie2 <- data.frame(date = as.Date(sau_sem_france_dep_sts@epoch), upp = sau_sem_france_dep_sts@upperbound)
# On enlève les bornes supérieures égales à 0
serie2 <- serie2[which(serie2$upp != 0),]
serie2 <- xts(serie2$upp, order.by = serie2$date)

# Fusion des deux séries
serie <- cbind(serie1, serie2)
names(serie) <- c("cas_obs", "borne_sup")

# Création du graphique dypgrah avec la borne supérieure de prédiction
dygraph_sau <- dygraph(serie,
                       main = "Nombre de passages pour une ischémie myocardique (SAU) pour 10 000 passages codés entre janvier 2010 et décembre 2017 sur Paris pour tous âges et seuil (méthode Farrington)",
                       xlab = "Date",
                       ylab = "Nombre de passages pour 10 000") %>%
  dySeries("cas_obs", label = "Proportion observée") %>%
  dySeries("borne_sup", label = "Borne supérieure de détection", strokePattern = "dashed") %>%
  dyOptions(drawGrid = FALSE) %>%
  dyRangeSelector()
dygraph_sau

# On rajoute au graphique dygraph les alarmes
for(i in seq_along(sau_sem_france_dep_sts@alarm)){
  
  if(sau_sem_france_dep_sts@alarm[i] == 1){
    dygraph_sau <- dygraph_sau %>% dyAnnotation(x = as.Date(sau_sem_france_dep_sts@epoch[i], origin = "1970-01-01"), text = "A",
                                                tooltip = as.character(sau_sem_france_dep_sts@observed[i]))
  }
}
dygraph_sau


##### EARS C2

# NOTE : Cet algorithe utilise un objet STS et pas un objet Disprog
# On accède aux colonnes avec "@" et non pas avec "$"

# Application de l'algorithme EARS C2
sau_sem_france_dep_earsc2 <- earsC(sts = sau_sem_france_dep_sts,
                                   control = list(method = "C2", range = c(min_sem: nbr_sem))) # Période que l'on veut étudier

# On remplit la colonne @alarm avec les alarmes calculées par l'algorithme
# Faire attention au "@" à l'intérieur de la fonction
sau_sem_france_dep_sts@alarm <- as.matrix(c(rep(0, times = min_sem-1), sau_sem_france_dep_earsc2@alarm))

# On remplit la colonne @upperbound avec les alarmes calculées par l'algorithme
# Faire attention au "@" à l'intérieur de la fonction
sau_sem_france_dep_sts@upperbound <- as.matrix(c(rep(0, times = min_sem - 1), sau_sem_france_dep_earsc2@upperbound))

# Création de la série temporelle avec les cas observés
serie1 <- data.frame(date = as.Date(sau_sem_france_dep_sts@epoch), obs = sau_sem_france_dep_sts@observed)
serie1 <- xts(serie1$observed1, order.by = serie1$date)

# Créationd de la série temporelle avec les bornes supérieures
serie2 <- data.frame(date = as.Date(sau_sem_france_dep_sts@epoch), upp = sau_sem_france_dep_sts@upperbound)
# On enlève les bornes supérieures égales à 0
serie2 <- serie2[which(serie2$upp != 0),]
serie2 <- xts(serie2$upp, order.by = serie2$date)

# Fusion des deux séries
serie <- cbind(serie1, serie2)
names(serie) <- c("cas_obs", "borne_sup")

# Création du graphique dypgrah avec la borne supérieure de prédiction
dygraph_sau <- dygraph(serie,
                       main = "Nombre de passages pour une ischémie myocardique (SAU) pour 10 000 passages codés entre janvier 2010 et décembre 2017 sur Paris pour tous âges et seuil (méthode EARS C2)",
                       xlab = "Date",
                       ylab = "Nombre de passages pour 10 000") %>%
  dySeries("cas_obs", label = "Proportion observée") %>%
  dySeries("borne_sup", label = "Borne supérieure de détection", strokePattern = "dashed") %>%
  dyOptions(drawGrid = FALSE) %>%
  dyRangeSelector()
dygraph_sau

# On rajoute au graphique dygraph les alarmes
for(i in seq_along(sau_sem_france_dep_sts@alarm)){
  
  if(sau_sem_france_dep_sts@alarm[i] == 1){
    dygraph_sau <- dygraph_sau %>% dyAnnotation(x = as.Date(sau_sem_france_dep_sts@epoch[i], origin = "1970-01-01"), text = "A",
                                                tooltip = as.character(sau_sem_france_dep_sts@observed[i]))
  }
}
dygraph_sau
