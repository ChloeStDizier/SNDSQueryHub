##############################################################
#                                                            #
#   Identification des séjours en psychiatrie (RIM-P)        #
#      selon une liste de diagnostics prédfinie              #
#                                                            #
#                                                            #
##############################################################

# A LIRE ----
# Les tables produites ne sontpas stockés en dehors de l'environnement 
# Si cela est nécessaire, il vous revient d'écrire ces tables :
# 1) dans Orauser avec dbWriteTable
# 2) en rds ou csv dans l'espace de travail
# Ce stockage doit toujours être temporaire et sur la plateforme



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
  
  
  if (an < 2020) {
    # Avant 2020, durée de sejour = SEJ_DUR
    
    
    extract = tbl(conn, paste0("T_RIP", an - 2000, "RSA")) %>% 
      select(RIP_NUM, ETA_NUM_EPMSI, AGE_ANN, COD_SEX, DGN_PAL) %>% 
      left_join(tbl(conn, paste0("T_RIP", an - 2000, "RSAD")), by = c("ETA_NUM_EPMSI", "RIP_NUM")) %>% 
      filter(ASS_DGN %in% diag_liste | DGN_PAL %in% diag_liste) %>% 
      left_join(tbl(conn, paste0("T_RIP", an - 2000, "FB")), by = c("ETA_NUM_EPMSI", "RIP_NUM")) %>% 
      left_join(tbl(conn, paste0("T_RIP", an - 2000, "C")), by = c("ETA_NUM_EPMSI", "RIP_NUM")) %>% 
      select(RIP_NUM, ETA_NUM_EPMSI, AGE_ANN, COD_SEX, SEJ_DUR, DGN_PAL, NIR_ANO_17, EXE_SOI_DTD) %>% 
      collect()
    
    cat("Done\n")
    
    
    # Création du tableau avec une ligne par séjour pour l'année an
    cat("---- Reconstitution des séjours ...\n")
    
    sej = extract %>% 
      group_by(RIP_NUM, ETA_NUM_EPMSI, NIR_ANO_17) %>% 
      summarise(duree = max(SEJ_DUR, na.rm = T),
                age = max(AGE_ANN, na.rm = T),
                sexe = max(COD_SEX, na.rm = T),
                date = min(EXE_SOI_DTD, na.rm = T)) %>% 
      mutate(annee = an)
    
  } else {
    # Après 2020, durée de sejour = ENT_DEL_DAT
    
    
    extract = tbl(conn, paste0("T_RIP", an - 2000, "RSA")) %>% 
      select(RIP_NUM, ETA_NUM_EPMSI, AGE_ANN, COD_SEX, DGN_PAL) %>% 
      left_join(tbl(conn, paste0("T_RIP", an - 2000, "RSAD")), by = c("ETA_NUM_EPMSI", "RIP_NUM")) %>% 
      filter(ASS_DGN %in% diag_liste | DGN_PAL %in% diag_liste) %>% 
      left_join(tbl(conn, paste0("T_RIP", an - 2000, "FB")), by = c("ETA_NUM_EPMSI", "RIP_NUM")) %>% 
      select(-EXE_SOI_DTD) %>% 
      left_join(tbl(conn, paste0("T_RIP", an - 2000, "C")), by = c("ETA_NUM_EPMSI", "RIP_NUM")) %>% 
      select(RIP_NUM, ETA_NUM_EPMSI, AGE_ANN, COD_SEX, SEJ_DUR, DGN_PAL, NIR_ANO_17, EXE_SOI_DTD) %>% 
      collect()
    
    
    cat("Done\n")
    
    
    # Création du tableau avec une ligne par séjour pour l'année an
    cat("---- Reconstitution des séjours ...\n")
    
    sej = extract %>% 
      group_by(RIP_NUM, ETA_NUM_EPMSI, NIR_ANO_17) %>% 
      summarise(duree = max(DEL_DAT_ENT, na.rm = T),
                age = max(AGE_ANN, na.rm = T),
                sexe = max(COD_SEX, na.rm = T), 
                date = min(EXE_SOI_DTD, na.rm = T)) %>% 
      mutate(annee = an)    
    
  }
  
  
  cat(paste("Done :", nrow(sej), "séjours en", an, "\n"))
  
  # Ajout dans le df complet
  sejour_inclus = rbind(sejour_inclus,sej)
  
  
  # Récupération de tout les diag correspondant aux séjours inclus
  cat("------ Récupération des diagnostics ...\n")
  
  diag = extract %>% 
    select(ETA_NUM_EPMSI ,RIP_NUM, NIR_ANO_17, diag = DGN_PAL) %>% 
    rbind(extract %>% 
            left_join(tbl(conn, paste0("T_RIP", an - 2000, "RSAD")),
                      by = c("ETA_NUM_EPMSI", "RIP_NUM"), copy = T) %>%
            select(ETA_NUM_EPMSI, RIP_NUM, NIR_ANO_17, diag = ASS_DGN)) %>%
    group_by(ETA_NUM_EPMSI, RIP_NUM, NIR_ANO_17, diag) %>% 
    summarise(n = n()) %>% 
    select(-n) %>% 
    mutate(annee = an)
  
  cat(paste("Done :", nrow(diag), "diagnostics récupérés pour", an, "\n"))
  
  # Ajout dans le df complet
  diag_inclus = rbind(diag_inclus, diag)
  
  cat("\n\n")
  
}
