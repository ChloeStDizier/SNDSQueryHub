###########################################################
#                                                         #
#              BOUCLE D'INCLUSION DE SEQUENCES DE         #
#                   DELIVRANCES EN PHARMACIE              #
#                                                         #
#     Objectif : A partir d'une table de delivrances      #
#     complete pour une periode donnee
#                                                         #
#    1. inclure les primo delivrances (i.e. sans autres   #
#       delivrances pendant X jours)                      #
#    2. inclure toutes les delivrances suivantes sur      #
#       une periode de suivi de X jours                   #
#                                                         #
#             Version 1 : 09/04/2025                      #
#                                                         #
#         Chloé Saint-Dizier - F2RSM PSY                  #
#                                                         #
###########################################################


# A LIRE ----
# Ce script est a utiliser a partir d'une table stockee dans ORAUSER
# La table doit contenir un ensemble de delivrances avec au minimu les variables suivantes :
##  BEN_NIR_PSA, BEN_RNG_GEM, BEN_SEX_COD
##  EXE_SOI_DTD, PHA_PRS_C13, PSP_SPE_COD


# 00 - Connexion à la base ORACLE -----

source("connect.R")  # charger le fichier dedie



# 01 - Parametrage ----

## Nom de la table dans ORAUSER ----

# Completer table par le nom de la table a traiter dans ORAUSER (en majsucules)

table = 

## Periode d'inclusion ----

# Completer debut_inclusion et fin_inclusion avec les années correspondantes

debut_inclusion = 
fin_inclusion =


## Periode sans antecedents ----

# Completer purge par le nombre de jour souhaite pour considerer 
# une delivrance comme "primo delivrance"
# Pour ne pas tenir compte des antecedents, noter purge = 0

purge =

## Periode de suivi ----

# Completer suivi par la duree de suivi (en jours) souhaitee a partir
# de la premiere delivrance incluse
# exemple : si suivi = 365, toutes les delivrances dans les 365 jours
# qui suivent la primo delivrance seront incluses 
# Pour n'inclure que les primo delivrances : suivi = 0

suivi =


## Chemin de stockage en rds ----

# Les tables doivent être stockés temporairement en rds dans l'espace de travail
# Indiquer un chemin dans chemin

chemin = 
  
  
# 02 - Calcul de variables auxiliaires ----

## Calcul du nombre d'annees a parcourir pour couvrir la totalite de la purge
annee_purge = (purge%/%366) + 1


## Calcul du nombre d'annees a parcourir pour couvrir la totalite du suivi 
annee_suivi = (suivi%/%366) + 1
  


# 03 - Boucle ----


for (i in c(debut_inclusion:fin_inclusion)){
  
  
  print(paste0("\n _____ ANNEE ",i," _____ \n"))
  
  
  ## Liste des patients avec au moins une delivrance dans l'annee i ----
  
  cat("-- Calcul du nombre de patients ... \n")
  
  patient_i =  dbGetQuery(conn,
                          paste0("select BEN_NIR_PSA, BEN_RNG_GEM, EXE_SOI_DTD
                          from ", table,
                          "where EXTRACT(YEAR FROM EXE_SOI_DTD) =",i)) %>% 
    mutate(id = paste(BEN_NIR_PSA, BEN_RNG_GEM, sep = "_")) %>% 
    group_by(id) %>% 
    summarise(date_min = min(EXE_SOI_DTD)) %>% 
    collect()
  
  cat("...Done :", nrow(patient_i), "patients \n")
  
  
  
  ## Recherche des primo delivrances ----
  
  cat("---- Recherche des primo delivrances ... \n")
  
  # Creation de la liste d'annees a parcourir pour la purge
  
  liste_annee_purge = seq(i-1, i-annee_purge)
  liste_annee_purge_chr = paste0("(", paste(liste_annee_purge, collapse = ','), ')')
  
  exclusion_i = dbGetQuery(conn,
                            paste0("select BEN_NIR_PSA, BEN_RNG_GEM, EXE_SOI_DTD
                            from ", table,
                            "where EXTRACT(YEAR FROM EXE_SOI_DTD) in" , liste_annee_purge_chr)) %>%  
    mutate(id = paste(BEN_NIR_PSA, BEN_RNG_GEM, sep = "_")) %>% 
    group_by(id) %>% 
    summarise(date_max = max(EXE_SOI_DTD)) %>% 
    left_join(patient_i, by = "id") %>% 
    mutate(delai = as.numeric(difftime(date_min, date_max, units = "days"))) %>% 
    filter(delai <= purge) %>% collect()
  
  
  cat("... Done",nrow(patient_i) - nrow(exclusion_i), "primo delivrances \n")
  
  
  
  cat("-------- Inclusion des delivrances dans la periode de suivi ... \n")
  
  
  # Creation de la liste d'annees a parcourir pour la purge
  
  liste_annee_suivi = seq(i + 1, i + annee_suivi)
  liste_annee_suivi_chr = paste0("(", paste(liste_annee_suivi, collapse = ','), ')')
  
  
  delivrances_i = dbGetQuery(conn,
                          paste0("select BEN_NIR_PSA, BEN_RNG_GEM,BEN_NAI_ANN, BEN_SEX_COD, EXE_SOI_DTD, PHA_PRS_C13, PSP_SPE_COD
                          from ", table, 
                          "where EXTRACT(YEAR FROM EXE_SOI_DTD) in ", liste_annee_suivi_chr)) %>% 
    mutate(id = paste(BEN_NIR_PSA, BEN_RNG_GEM, sep = "_")) %>% 
    right_join(patient_i, by = "id") %>% 
    filter(!id %in% exclusion_i$id) %>% 
    mutate(delai = as.numeric(difftime(EXE_SOI_DTD, date_min, units = "days"))) %>% 
    filter(delai <= suivi) %>% collect() %>% 
    mutate(id_seq = paste(id, i, sep = "_"))
  
  nb_seq = nrow(delivrances_i %>% select(id_seq) %>% distinct())
  
  cat("... Done :", nrow(delivrances_i), "delivrances (", nb_seq, "sequences) \n")
  
  rm(exclusion_i)
  rm(patient_i)
  
  saveRDS(delivrances_i, paste0(chemin,"/seq_",i,".rds"))
  
  cat("---------- Sequences enregistres dans", paste0(chemin,"/seq_",i,".rds"))
  cat("\n\n")
  
}


# 04 - Concatenation des données annuelles ----

## Recuperation de la liste de fichier ----

liste_rds = list.files(path = chemin,
                       pattern = "seq_[0-9][0-9][0-9][0-9].rds")


## Creation du data frame vide ----

data = data.frame()

## Lecture des fichiers annuels et concatenation ----

for (f in liste_rds) {
  
  cat("Ajout de", f, "...\n")
  
  chemin_f = file.path(chemin, f)
  
  data_new = readRDS(chemin_f)
  
  data = rbind(data, data_new)
  
  cat("... Done \n")
  cat("--> Nombre de lignes total  :", nrow(data))
  cat("\n")
  
}


# 05 - Ecriture de la table finale et suppression des données annuelles

# /!\ ATTENTION /!\ 
# A realiser uniquement apres avoir verification de l'etape 04
# En cas d'erreur, la totalite du script sera a executer a nouveau apres
# suppression des donnees annuelles 


## Ecriture en rds ----

# Le fichier devra etre supprime lorsque les analyses seront terminees

saveRDS(data, paste0(chemin,"/data_complete.rds"))



## Suppression des donnees annuelles ----

for (f in liste_rds) {

  chemin_f = file.path(chemin, f)
  
  file.remove(chemin_f)
  
  cat(f, "supprime \n")
  
}
