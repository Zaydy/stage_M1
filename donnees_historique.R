# Script permettant de traiter les données SurSaUD et de les stocker dans des tables SQL

# Répertoire de travail


# Installation et chargement des packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman, data.table, dplyr, stringr, foreach, MASS, doParallel, ISOweek, RMySQL)
source( "include/mysql.inc.R", encoding="UTF-8" )

# Lecture des données compilées d'origine (période 2010 à 2017)
data_sursaud <- readRDS("./data/data_sursaud_histo.rds")

# Lecture du fichier fournisseurs
fournisseurs <- fread("./data/fournisseurs.txt", select = 1:8, colClasses=list(character=c(6,7)), encoding = "UTF-8")

# Ré-écriture du code départemental de la Corse
fournisseurs[V7== 20 & V4 == "SAU" ,V7 := substr(V5, 1, 2)]
fournisseurs[V7== 20 & V4 == "SOS" ,V7 := ifelse(V6 == "20A","2A","2B")]
setnames(fournisseurs, old=c("V2","V7"), new = c("Identifiant_etablissement","code_dep"))

# Conservation uniquement de l'id de l'établissement et du code départemental
fournisseurs <- fournisseurs[, .(Identifiant_etablissement, code_dep)]

# Formatage des dates (conversion des character en Date)
data_sursaud[, Date_de_passage := as.Date(Date_de_passage, "%d/%m/%y")]

# Lecture du ficher des codes region et departement
# Liste des codes region et departement selon l'INSEE
# INSEE : https://www.insee.fr/fr/information/3363419
code_reg_dep <- fread("./data/corresp_dep_reg.csv", header = TRUE, encoding = "UTF-8")
setnames(code_reg_dep, old = c("REGION", "DEP"), new = c("code_reg", "code_dep"))

# On récupère le code_reg pour fournisseurs avec la fusion
fournisseurs <- merge(x = fournisseurs, y = code_reg_dep, by = "code_dep", all.x = TRUE)

# Codes des DOM TOM
code_doms <- c(971, 972, 973, 974, 975, 976, 977, 978)

# Fonction pour calculer le lundi de la semaine courante
prevM <- function(x){
  7 * floor(as.numeric(x-1+4) / 7) + as.Date(1-4, origin = "1970-01-01")
}


##### Agrégation par jour et par département #####

# Fusion pour récupérer le code_dep
data_jour_france_dep <- merge(x = data_sursaud, y = fournisseurs[, .(Identifiant_etablissement, code_dep)], by = "Identifiant_etablissement")

# Somme des du nombre de passages par dep, RS, age et date
data_jour_france_dep <- data_jour_france_dep[, lapply(.SD, sum, na.rm = TRUE),
                                     by = .(Identifiant_classe_age, Identifiant_regroupement_syndromique, Date_de_passage, code_dep),
                                     .SDcols = c("Nombre_de_passage")]

# Récupération de TOUS_T_DSO, TOUS_T_DRP de chaque combinaison : Id_etab, ID_classe_age, Id_RS, Date_de_passage
tous_actes_codes <- data_jour_france_dep[Identifiant_regroupement_syndromique %in% c("TOUS_T_DSO", "TOUS_T_DRP")]

# Récupération des totaux "TOUS_T_DSO" et "TOUS_T_DRP" en colonnes
tous_actes_codes <- dcast(data = tous_actes_codes,
                          formula = code_dep + Identifiant_classe_age + Date_de_passage ~ Identifiant_regroupement_syndromique,
                          value.var = "Nombre_de_passage")

# On enlève les lignes d'Id_RS contenant "TOUS_T_DSO" et "TOUS_T_DRP"
data_jour_france_dep <- data_jour_france_dep[!(Identifiant_regroupement_syndromique %in% c("TOUS_T_DSO", "TOUS_T_DRP"))]

# Création d'une colonne Source pour connaître la provenance des données : SAU ou SOS
data_jour_france_dep[, Source := ifelse(grepl(pattern = "^DSO\\d{4}$", x = Identifiant_regroupement_syndromique), yes = "SOS", no = "SAU")]

# Fusion pour récupérer les totaux
data_jour_france_dep <- merge(x = data_jour_france_dep, y = tous_actes_codes, by = c("code_dep", "Identifiant_classe_age", "Date_de_passage"), all.x = TRUE)

# Création d'une colonne : tous_actes_codes
# Si SAU on récupère la valeur dans "TOUS_T_DRP" sinon dans "TOUS_T_DSO"
data_jour_france_dep[, total_actes_codes := ifelse(Source == "SAU", yes = TOUS_T_DRP, no = TOUS_T_DSO)]

# Suppression des deux colonnes TOUS_T_DRP et TOUS_T_DSO
data_jour_france_dep[, c("TOUS_T_DRP", "TOUS_T_DSO") := NULL]

# ### Pour SAU ###
# 
# ## Dans toute la France ##
# sau_jour_france_dep <- data_jour_france_dep[Source == "SAU"]
# 
# ## Dans les DOM TOM ##
# sau_jour_dom_dep <- sau_jour_france_dep[code_dep %in% code_doms]
# 
# ## Dans la France métropolitaine ##
# sau_jour_metro_dep <- sau_jour_france_dep[!code_dep %in% code_doms]
# 
# 
# ### Pour SOS Médecins ###
# 
# ## Dans toute la France ##
# sos_jour_france_dep <- data_jour_france_dep[Source == "SOS"]
# 
# ## Dans les DOM TOM ##
# sos_jour_dom_dep <- sos_jour_france_dep[code_dep %in% code_doms]
# 
# ## Dans la France métropolitaine ##
# sos_jour_metro_dep <- sos_jour_france_dep[!code_dep %in% code_doms]


##### Agrégation par jour et par région #####

# Fusion pour récupérer le code_reg
data_jour_france_reg <- merge(x = data_sursaud, y = fournisseurs[, .(Identifiant_etablissement, code_reg)], by = "Identifiant_etablissement")

# Somme des passages par dep, RS, age et date
data_jour_france_reg <- data_jour_france_reg[, lapply(.SD, sum, na.rm = TRUE),
                                             by = .(Identifiant_classe_age, Identifiant_regroupement_syndromique, Date_de_passage, code_reg),
                                             .SDcols = c("Nombre_de_passage")]

# Récupération de TOUS_T_DSO, TOUS_T_DRP de chaque combinaison : Id_etab, ID_classe_age, Id_RS, Date_de_passage
tous_actes_codes <- data_jour_france_reg[Identifiant_regroupement_syndromique %in% c("TOUS_T_DSO", "TOUS_T_DRP")]

# Récupération des totaux "TOUS_T_DSO" et "TOUS_T_DRP" en colonnes
tous_actes_codes <- dcast(data = tous_actes_codes,
                          formula = code_reg + Identifiant_classe_age + Date_de_passage ~ Identifiant_regroupement_syndromique,
                          value.var = "Nombre_de_passage")

# On enlève les lignes d'Id_RS contenant "TOUS_T_DSO" et "TOUS_T_DRP"
data_jour_france_reg <- data_jour_france_reg[!(Identifiant_regroupement_syndromique %in% c("TOUS_T_DSO", "TOUS_T_DRP"))]

# Création d'une colonne Source pour connaître la provenance des données : SAU ou SOS
data_jour_france_reg[, Source := ifelse(grepl(pattern = "^DSO\\d{4}$", x = Identifiant_regroupement_syndromique), yes = "SOS", no = "SAU")]

# On remplace les NA dans code_reg par 0
data_jour_france_reg[is.na(code_reg), code_reg := 0]
tous_actes_codes[is.na(code_reg), code_reg := 0]

# Fusion pour récupérer les totaux
data_jour_france_reg <- merge(x = data_jour_france_reg, y = tous_actes_codes, by = c("code_reg", "Identifiant_classe_age", "Date_de_passage"), all.x = TRUE)


# Création d'une colonne : tous_actes_codes
# Si SAU on récupère la valeur dans "TOUS_T_DRP" sinon dans "TOUS_T_DSO"
data_jour_france_reg[, total_actes_codes := ifelse(Source == "SAU", yes = TOUS_T_DRP, no = TOUS_T_DSO)]

# Suppression des deux colonnes TOUS_T_DRP et TOUS_T_DSO
data_jour_france_reg[, c("TOUS_T_DRP", "TOUS_T_DSO") := NULL]



# ### Pour SAU ###
# 
# ## Dans toute la france ##
# sau_jour_france_reg <- data_jour_france_reg[Source == "SAU"]
# 
# ## Dans les DOM TOM ##
# sau_jour_dom_reg <- sau_jour_france_reg[nchar(code_reg) != 2 | is.na(nchar(code_reg))]
# 
# ## Dans la France métropolitaine ##
# sau_jour_metro_reg <- sau_jour_france_reg[nchar(code_reg) == 2]
# 
# 
# ### Pour SOS ###
# 
# ## Dans toute la france ##
# sos_jour_france_reg <- data_jour_france_reg[Source == "SOS"]
# 
# ## Dans les DOM TOM ##
# sos_jour_dom_reg <- sos_jour_france_reg[nchar(code_reg) != 2 | is.na(nchar(code_reg))]
# 
# ## Dans la France métropolitaine ##
# sos_jour_metro_reg <- sos_jour_france_reg[nchar(code_reg) == 2]


#### Agrégation par semaine et par département ####

# Fusion pour récupérer le code_dep
data_sem_france_dep <- merge(x = data_sursaud, y = fournisseurs[, .(Identifiant_etablissement, code_dep)], by = "Identifiant_etablissement")

# Récupération du numéro des semaines
data_sem_france_dep[, first_monday := prevM(Date_de_passage)]

# Suppression de la colonne Date_de_passage
data_sem_france_dep[, Date_de_passage := NULL]

# Somme des passages par dep, RS, age et date
data_sem_france_dep <- data_sem_france_dep[, lapply(.SD, sum, na.rm = TRUE),
                                       by = .(Identifiant_classe_age, Identifiant_regroupement_syndromique, first_monday, code_dep),
                                       .SDcols = c("Nombre_de_passage")]

# Récupération de TOUS_T_DSO, TOUS_T_DRP de chaque combinaison : Id_etab, ID_classe_age, Id_RS, Date_de_passage
tous_actes_codes <- data_sem_france_dep[Identifiant_regroupement_syndromique %in% c("TOUS_T_DSO", "TOUS_T_DRP")]

# Récupération des totaux "TOUS_T_DSO" et "TOUS_T_DRP" en colonnes
tous_actes_codes <- dcast(data = tous_actes_codes,
                          formula = code_dep + Identifiant_classe_age + first_monday ~ Identifiant_regroupement_syndromique,
                          value.var = "Nombre_de_passage")

# On enlève les lignes d'Id_RS contenant "TOUS_T_DSO" et "TOUS_T_DRP"
data_sem_france_dep <- data_sem_france_dep[!(Identifiant_regroupement_syndromique %in% c("TOUS_T_DSO", "TOUS_T_DRP"))]

# Création d'une colonne Source pour connaître la provenance des données : SAU ou SOS
data_sem_france_dep[, Source := ifelse(grepl(pattern = "^DSO\\d{4}$", x = Identifiant_regroupement_syndromique), yes = "SOS", no = "SAU")]

# Fusion pour récupérer les totaux
data_sem_france_dep <- merge(x = data_sem_france_dep, y = tous_actes_codes, by = c("code_dep", "Identifiant_classe_age", "first_monday"), all.x = TRUE)


# Création d'une colonne : tous_actes_codes
# Si SAU on récupère la valeur dans "TOUS_T_DRP" sinon dans "TOUS_T_DSO"
data_sem_france_dep[, total_actes_codes := ifelse(Source == "SAU", yes = TOUS_T_DRP, no = TOUS_T_DSO)]

# Suppression des deux colonnes TOUS_T_DRP et TOUS_T_DSO
data_sem_france_dep[, c("TOUS_T_DRP", "TOUS_T_DSO") := NULL]

# ### Pour SAU ###
# 
# ## Dans toute la France ##
# sau_sem_france_dep <- data_sem_france_dep[Source == "SAU"]
# 
# ## Dans les DOM TOM ##
# sau_sem_dom_dep <- sau_sem_france_dep[code_dep %in% code_doms]
# 
# ## Dans la France métropolitaine ##
# sau_sem_metro_dep <- sau_sem_france_dep[!code_dep %in% code_doms]
# 
# 
# ### Pour SOS Médecins ###
# 
# ## Dans toute la France ##
# sos_sem_france_dep <- data_sem_france_dep[Source == "SOS"]
# 
# ## Dans les DOM TOM ##
# sos_sem_dom_dep <- sos_sem_france_dep[code_dep %in% code_doms]
# 
# ## Dans la France métropolitaine ##
# sos_sem_metro_dep <- sos_sem_france_dep[!code_dep %in% code_doms]


#### Agrégation par semaine et par région ####

# Fusion pour récupérer le code_reg
data_sem_france_reg <- merge(x = data_sursaud, y = fournisseurs[, .(Identifiant_etablissement, code_reg)], by = "Identifiant_etablissement")

# Récupération du numéro des semaines
data_sem_france_reg[, first_monday := prevM(Date_de_passage)]

# Suppression de la colonne Date_de_passage
data_sem_france_reg[, Date_de_passage := NULL]

# Somme des passages par dep, RS, age et date
data_sem_france_reg <- data_sem_france_reg[, lapply(.SD, sum, na.rm = TRUE),
                             by = .(Identifiant_classe_age, Identifiant_regroupement_syndromique, first_monday, code_reg),
                             .SDcols = c("Nombre_de_passage")]

# Récupération de TOUS_T_DSO, TOUS_T_DRP de chaque combinaison : Id_etab, ID_classe_age, Id_RS, Num_semaine
tous_actes_codes <- data_sem_france_reg[Identifiant_regroupement_syndromique %in% c("TOUS_T_DSO", "TOUS_T_DRP")]

# Récupération des totaux "TOUS_T_DSO" et "TOUS_T_DRP" en colonnes
tous_actes_codes <- dcast(data = tous_actes_codes,
                          formula = code_reg + Identifiant_classe_age + first_monday ~ Identifiant_regroupement_syndromique,
                          value.var = "Nombre_de_passage")

# On enlève les lignes d'Id_RS contenant "TOUS_T_DSO" et "TOUS_T_DRP"
data_sem_france_reg <- data_sem_france_reg[!(Identifiant_regroupement_syndromique %in% c("TOUS_T_DSO", "TOUS_T_DRP"))]

# Création d'une colonne Source pour connaître la provenance des données : SAU ou SOS
data_sem_france_reg[, Source := ifelse(grepl(pattern = "^DSO\\d{4}$", x = Identifiant_regroupement_syndromique), yes = "SOS", no = "SAU")]

# On remplace les NA dans code_reg par 0
data_sem_france_reg[is.na(code_reg), code_reg := 0]
tous_actes_codes[is.na(code_reg), code_reg := 0]

# Fusion pour récupérer les totaux
data_sem_france_reg <- merge(x = data_sem_france_reg, y = tous_actes_codes, by = c("code_reg", "Identifiant_classe_age", "first_monday"), all.x = TRUE)


# Création d'une colonne : tous_actes_codes
# Si SAU on récupère la valeur dans "TOUS_T_DRP" sinon dans "TOUS_T_DSO"
data_sem_france_reg[, total_actes_codes := ifelse(Source == "SAU", yes = TOUS_T_DRP, no = TOUS_T_DSO)]

# Suppression des deux colonnes TOUS_T_DRP et TOUS_T_DSO
data_sem_france_reg[, c("TOUS_T_DRP", "TOUS_T_DSO") := NULL]

# ### Pour SAU ###
# 
# ## Dans toute la france ##
# sau_sem_france_reg <- data_sem_france_reg[Source == "SAU"]
# 
# ## Dans les DOM TOM ##
# sau_sem_dom_reg <- sau_sem_france_reg[nchar(code_reg) != 2 | is.na(nchar(code_reg))]
# 
# ## Dans la France métropolitaine ##
# sau_sem_metro_reg <- sau_sem_france_reg[nchar(code_reg) == 2]
# 
# 
# ### Pour SOS ###
# 
# ## Dans toute la france ##
# sos_sem_france_reg <- data_sem_france_reg[Source == "SOS"]
# 
# ## Dans les DOM TOM ##
# sos_sem_dom_reg <- sos_sem_france_reg[nchar(code_reg) != 2 | is.na(nchar(code_reg))]
# 
# ## Dans la France métropolitaine ##
# sos_sem_metro_reg <- sos_sem_france_reg[nchar(code_reg) == 2]


#### Partie BDD ####

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Function to connect to the database
connect <- function(){
  ## Si on est sur PC, on se connecte à intr, ison à l'hôte local
  #host <- ifelse(  Sys.getenv("OS")=='Windows_NT', 'intr', 'localhost' )
  host <- 'localhost'
  
  dbConnect(drv = RMySQL::MySQL(), host = host, dbname= "data_sursaud_surveillance", username = "root", password = "")
}

# Connexion globale à la BDD
con <- connect()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~ Préparation des noeuds pour le calcul paralelle ~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#Nombre de noeuds
nodes <- detectCores()

# Création et enregistrement du cluster
cl <- makeCluster( nodes )
registerDoParallel( cl )  
#getDoParWorkers()

# Création des connexions à la base SQL sur chaque noeud
clusterExport( cl, varlist="connect" )
clusterEvalQ( cl, {
  library( RMySQL )
  con <- connect()
  NULL
})

#Préparation avant calcul parallèle
liste_source <- c("sau", "sos")
liste_zone <- c("france", "metro", "dom")

#Nombre de paquets
n <- 1000

# Choix des données que l'on veut mettre dans la BDD
data_jour_france_dep <- readRDS(file = "./data/data_jour_france_dep.rds")
# data_jour_france_reg <- readRDS(file = "./data/data_jour_france_reg.rds")
# data_sem_france_dep <- readRDS(file = "./data/data_sem_france_dep.rds")
# data_sem_france_reg <- readRDS(file = "./data/data_sem_france_reg.rds")


#Par jour, par département

noms_col <- c("Source", "Identifiant_regroupement_syndromique", "Identifiant_classe_age",
              "Date_de_passage", "code_dep", "Nombre_de_passage", "total_actes_codes")

setcolorder(data_jour_france_dep, noms_col)
setorderv(data_jour_france_dep, c("Source", "code_dep", "Identifiant_classe_age", "Date_de_passage"))
data_jour_france_dep[, Date_de_passage := as.character(Date_de_passage)]


for(k in seq_along(liste_source)){
  so <- liste_source[k]
  for(zone in liste_zone){
      if(zone == "dom"){
        data_temp <- data_jour_france_dep[code_dep %in% code_doms]
      } else if (zone == "metro") {
        data_temp <- data_jour_france_dep[!code_dep %in% code_doms]
      } else {
        data_temp <- data_jour_france_dep
      }
    
    data_temp[, index := .I]
    tab_recap <- data_temp[ , .(N =.N, min = min(index)), by=Source]
    mytable <- paste(so, "jour", zone, "dep", sep = "_")
    j <- ceiling( tab_recap$N[ k ] / n )
    
    foreach( i=1:j, .packages=c( 'data.table', 'RMySQL', 'DBI'), .noexport="con", .verbose=TRUE ) %dopar% {
      deb <- 1 + (i-1)*n
      fin <- min( i*n, tab_recap$N[k] )
      saveData( data_temp[ (deb:fin) + tab_recap$min[k]-1, 2:ncol(data_temp), with = FALSE ], mytable )
    }
  }
}


rm(data_jour_france_dep)
gc()
gc()


data_sem_france_dep <- readRDS(file = "./data/data_sem_france_dep.rds")


#Par semaine, par département

noms_col <- c("Source", "Identifiant_regroupement_syndromique", "Identifiant_classe_age",
              "first_monday", "code_dep", "Nombre_de_passage", "total_actes_codes")

setcolorder(data_sem_france_dep, noms_col)
setorderv(data_sem_france_dep, c("Source", "code_dep", "Identifiant_classe_age", "first_monday"))
data_sem_france_dep[, first_monday := as.character(first_monday)]


for(k in seq_along(liste_source)){
  so <- liste_source[k]
  for(zone in liste_zone){
    if(zone == "dom"){
      data_temp <- data_sem_france_dep[code_dep %in% code_doms]
    } else if (zone == "metro") {
      data_temp <- data_sem_france_dep[!code_dep %in% code_doms]
    } else {
      data_temp <- data_sem_france_dep
    }
    
    data_temp[, index := .I]
    tab_recap <- data_temp[ , .(N =.N, min = min(index)), by=Source]
    mytable <- paste(so, "sem", zone, "dep", sep = "_")
    j <- ceiling( tab_recap$N[ k ] / n )
    
    foreach( i=1:j, .packages=c( 'data.table', 'RMySQL', 'DBI'), .noexport="con", .verbose=TRUE ) %dopar% {
      deb <- 1 + (i-1)*n
      fin <- min( i*n, tab_recap$N[k] )
      saveData( data_temp[ (deb:fin) + tab_recap$min[k]-1, 2:ncol(data_temp), with = FALSE ], mytable )
    }
  }
}


rm(data_sem_france_dep)
gc()
gc()

data_jour_france_reg <- readRDS(file = "./data/data_jour_france_reg.rds")

#Par jour, par région

noms_col <- c("Source", "Identifiant_regroupement_syndromique", "Identifiant_classe_age",
              "Date_de_passage", "code_reg", "Nombre_de_passage", "total_actes_codes")

setcolorder(data_jour_france_reg, noms_col)
setorderv(data_jour_france_reg, c("Source", "code_reg", "Identifiant_classe_age", "Date_de_passage"))
data_jour_france_reg[, Date_de_passage := as.character(Date_de_passage)]


for(k in seq_along(liste_source)){
  so <- liste_source[k]
  for(zone in liste_zone){
    if(zone == "dom"){
      data_temp <- data_jour_france_reg[nchar(code_reg) != 2]
    } else if (zone == "metro") {
      data_temp <- data_jour_france_reg[nchar(code_reg) == 2]
    } else {
      data_temp <- data_jour_france_reg
    }
    
    data_temp[, index := .I]
    tab_recap <- data_temp[ , .(N =.N, min = min(index)), by=Source]
    mytable <- paste(so, "jour", zone, "reg", sep = "_")
    j <- ceiling( tab_recap$N[ k ] / n )
    
    foreach( i=1:j, .packages=c( 'data.table', 'RMySQL', 'DBI'), .noexport="con", .verbose=TRUE ) %dopar% {
      deb <- 1 + (i-1)*n
      fin <- min( i*n, tab_recap$N[k] )
      saveData( data_temp[ (deb:fin) + tab_recap$min[k]-1, 2:ncol(data_temp), with = FALSE ], mytable )
    }
  }
}

rm(data_jour_france_reg)
gc()

#Par semaine, par région

noms_col <- c("Source", "Identifiant_regroupement_syndromique", "Identifiant_classe_age",
              "first_monday", "code_reg", "Nombre_de_passage", "total_actes_codes")

setcolorder(data_sem_france_reg, noms_col)
setorderv(data_sem_france_reg, c("Source", "code_reg", "Identifiant_classe_age", "first_monday"))
data_sem_france_reg[, first_monday := as.character(first_monday)]


for(k in seq_along(liste_source)){
  so <- liste_source[k]
  for(zone in liste_zone){
    if(zone == "dom"){
      data_temp <- data_sem_france_reg[nchar(code_reg) != 2]
    } else if (zone == "metro") {
      data_temp <- data_sem_france_reg[nchar(code_reg) == 2]
    } else {
      data_temp <- data_sem_france_reg
    }
    
    data_temp[, index := .I]
    tab_recap <- data_temp[ , .(N =.N, min = min(index)), by=Source]
    mytable <- paste(so, "sem", zone, "reg", sep = "_")
    j <- ceiling( tab_recap$N[ k ] / n )
    
    foreach( i=1:j, .packages=c( 'data.table', 'RMySQL', 'DBI'), .noexport="con", .verbose=TRUE ) %dopar% {
      deb <- 1 + (i-1)*n
      fin <- min( i*n, tab_recap$N[k] )
      saveData( data_temp[ (deb:fin) + tab_recap$min[k]-1, 2:ncol(data_temp), with = FALSE ], mytable )
    }
  }
}

rm(data_sem_france_reg)
gc()

#~~~~~~~~~~~~ Closing connections to the SQL database ~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# globally 
dbDisconnect(con)

# on each node
clusterEvalQ( cl, {
  dbDisconnect( con )
  NULL
})

#~~~~~~~~~~~~ Stop the parallel cluster ~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
stopCluster( cl )
registerDoSEQ()
