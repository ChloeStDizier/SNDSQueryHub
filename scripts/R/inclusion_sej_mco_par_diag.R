##############################################################
#                                                            #
#   Identification des séjours en MCO selon une liste        #
#            de diagnostics prédfinie                        #
#                                                            #
#                                                            #
##############################################################

# A LIRE ----
# Les tables produites ne sontpas stockés en dehors de l'environnement 
# Si cela est nécessaire, il vous revient d'écrire ces tables :
# 1) dans Orauser avec dbWriteTable
# 2) en rds ou csv dans l'espace de travail
# Ce stockage doit toujours être temporaire et sur la plateformme



# Source ----

source("connect.R")


# 1) Paramètragess ----


# Première et dernière année d'inclusion
## A compléter 

annee_debut = 
annee_fin = 


# Liste de diagnostics servant de critère d'inclusion
## A completer 
## Exemple diag_liste = c("F061", "F202")
  
diag_liste = c()



# 2) Boucle ----

# Sortie :
## sejour_inclus  : dataframe avec une ligne par séjour 
# + age, sexe du patient, durée de séjour, diagnostic principal
## diag_inclus : dataframe contenant l'ensemble des diagnostics (principaux et associés)
# correspondant aux séjours inclus 


# Initialisation de dataframes vides
sejour_inclus = data.frame()

diag_inclus = data.frame()



for (an in c(annee_debut : annee_fin)) {
  
  cat(paste("ANNÉE", an, ":\n"))
  
  # Extraction pour l'année i selon les critères d'inclusion  
  cat("-- Inclusion ...\n")
  
  
  extract = tbl(conn, paste0("T_MCO", an - 2000, "B")) %>% 
    select(RSA_NUM, ETA_NUM, AGE_ANN, COD_SEX, SEJ_NBJ, DGN_PAL) %>% 
    left_join(tbl(conn, paste0("T_MCO", an - 2000, "D")), by = c("ETA_NUM", "RSA_NUM")) %>% 
    filter(ASS_DGN %in% diag_liste | DGN_PAL %in% diag_liste) %>% 
    left_join(tbl(conn, paste0("T_MCO", an - 2000, "C")), by = c("ETA_NUM", "RSA_NUM")) %>% 
    collect() %>% 
    select(RSA_NUM, ETA_NUM, AGE_ANN, COD_SEX, SEJ_NBJ, DGN_PAL, NIR_ANO_17) 
  
  cat("Done")
  
  
  # Création du tableau avec une ligne par séjour pour l'année an
  cat("---- Reconstitution des séjours ...\n")
  
  sej = extract %>% 
    group_by(RSA_NUM,ETA_NUM, NIR_ANO_17) %>% 
    summarise(duree = max(SEJ_NBJ, na.rm = T),
              age = max(AGE_ANN, na.rm = T),
              sexe = max(COD_SEX, na.rm = T)) %>% 
    mutate(annee = an)
  
  cat(paste("Done :", nrow(sej), "séjours en", an, "\n"))
  
  # Ajout dans le df complet
  sejour_inclus = rbind(sejour_inclus,sej)
  
  
  # Récupération de tout les diag correspondant aux séjours inclus
  cat("------ Récupération des diagnostics ...\n")
  
  diag = extract %>% 
    select(ETA_NUM,RSA_NUM, NIR_ANO_17, diag = DGN_PAL) %>% 
    rbind(extract %>% 
            left_join(tbl(conn, paste0("T_MCO", an - 2000, "D")),
                      by = c("ETA_NUM", "RSA_NUM"), copy = T) %>%
            select(ETA_NUM, RSA_NUM, NIR_ANO_17, diag = ASS_DGN)) %>%
    group_by(ETA_NUM,RSA_NUM, NIR_ANO_17, diag) %>% 
    summarise(n = n()) %>% 
    select(-n) %>% 
    mutate(annee = an)
  
  cat(paste("Done :", nrow(diag), "diagnostics récupérés pour", an, "\n"))
  cat("\n \n \n")
  
  # Ajout dans le df complet
  diag_inclus = rbind(diag_inclus, diag)
  
}
