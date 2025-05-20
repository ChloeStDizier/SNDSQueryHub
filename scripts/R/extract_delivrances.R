
##############################################################
#                                                            #
#  Extraction des delivrance pour une liste d'ATC donnee     #
#  par mois de mise à disposition sur une periode souhaitee  #
#                                                            #
#                version 2 : 08/04/2025                      #
#                                                            #
#            Chloé Saint-Dizier - F2RSM PSY                  #
#                                                            #
##############################################################

# A LIRE ----
# Les tables produites sont stockees dans ORAUSER cela implique :
# 1 - les tables doivent être supprimes à l'issue de la session de travail
# afin de liberer l'espace 
# 1 bis - En cas de requête particulierement longues, les tables peuvent être
# TEMPORAIREMENT conservees le temps de finaliser le projet (dans ce cas, 
# prevenir les autres utilisateurs reguliers)
# 2 - Attention, ne pas utiliser SAS pour poursuivre le travail sur ces tables,
# SAS ne permet l'acces qu'à une partie restreintes des donnees dans certains cas (sans avertissement)


# 00 - Connexion à la base Oracle ----

source("connect.R")  # charger le fichier dedie


# 01 - Recuperation des codes CIP13 associes aux ATC ----
# si l'on dispose directement des codes cip, cette etape est inutile

## Recuperation des CIP dans la table de nomenclature ----


liste_cip = dbGetQuery(conn,
                      "select distinct pha_cip_c13
                      from ir_pha_r a
                      where a.pha_atc_cla in ... ") 

# completer la clause where en remplaçant ... par la liste des ATC
# le format est le suivant : ('ATC01', 'ATC02', ... )
# des fonctions telles que SUBSTRING peuvent être utilisees


## Concatenation des codes CIP en chaine de caracteres ----
# on souhaite repoduire :  ( 'cip1', 'cip2', ...)

### Construction de la partie principale de la chaine de caracteres ----
aux = paste(liste_cip$PHA_CIP_C13, collapse = "','", sep = "")

### Correction aux extremites ----
liste_cip_chr = paste0("('", aux,"')")


# 02 - Construction de la requête SQL ----

sql = paste("SELECT PRS.BEN_NIR_PSA, PRS.BEN_RNG_GEM, PRS.EXE_SOI_DTD, PHA.PHA_PRS_C13, PRS.BEN_CMU_TOP, PRS.BEN_NAI_ANN, PRS.BEN_SEX_COD, PRS.PSP_SPE_COD
FROM ER_PRS_F PRS
INNER JOIN ER_PHA_F PHA 
ON PRS.FLX_DIS_DTD = PHA.FLX_DIS_DTD
AND PRS.FLX_EMT_ORD = PHA.FLX_EMT_ORD
AND PRS.FLX_EMT_NUM = PHA.FLX_EMT_NUM 
AND PRS.FLX_EMT_TYP = PHA.FLX_EMT_TYP
AND PRS.FLX_TRT_DTD = PHA.FLX_TRT_DTD
AND PRS.ORG_CLE_NUM = PHA.ORG_CLE_NUM
AND PRS.DCT_ORD_NUM = PHA.DCT_ORD_NUM
AND PRS.REM_TYP_AFF = PHA.REM_TYP_AFF
AND PRS.PRS_ORD_NUM = PHA.PRS_ORD_NUM
WHERE PHA.FLX_DIS_DTD =TO_DATE(':FLUX','YYYYMMDD')
AND EXE_SOI_DTD BETWEEN TO_DATE(':DEBSOIAAAAMMJJ','YYYYMMDD') AND  TO_DATE(':FINSOIAAAAMMJJ','YYYYMMDD')
AND PHA.PHA_PRS_C13 IN", liste_cip_chr)

# l'instruction select peut-etre mofifiee selon les besoins
# les lignes 56 à 64 sont les cles de jointure entre les deux tables necessaires
# les lignes 65 et 66 ne doivent pas être modifies pour permettre l'iteration par mois de mise à disposition
# la ligne 67 permet de filter selon les codes CIP precedemment definis
# la clause where peut-être completee par d'autres conditions si besoin (AND/OR) 


# 03 - Parametrage de la boucle ----

## Nom de la table ----

tbl_nom = "..."   # remplacer ... par le nom souhaite pour la table de sortie (dans orauser)

## Periode d'interêt ----

debut = "YYYYMMDD"
fin = "YYYYMMDD"
# completer avec les bornes de la periode d'inclusion au format souhaitee

date_max = "YYYYMMDD" 
# completer avec les mois de mise à disposition le plus recent à parcourir
# conseil : rajouter 6 mois à la periode d'inclusion

# exemple : etude sur les delivrances de 2022 ET 2023 :
## debut = "20220101"
## fin = "20231231"
## date_max = "20240630"


# 04 - Iteration par mois de mise à disposition ----

## Variables necessaire ----

### Formattage des chaines de caracteres en date ----
debut_date = as.Date(debut, format = "%Y%m%d")
fin_date = as.Date(fin, format="%Y%m%d")


### Definition des bornes pour les iterations ----


# les dates de la mise à dispo sont toujours au 1er du mois
# les iterations doivent commencer un mois apres le debut de la periode d'interet
# et finir au mois correspond à date_max

flx_min = make_date(year = year(debut_date), month = month(debut_date) + 1, day = 01)
flx_max = floor_date(as.Date(date_max, format = "%Y%m%d"),"month")

# construction de la liste de flux de mise à dispo à parcourir 
# calcul de sa longueur
vec_flx = seq(from = flx_min, to = flx_max, by='1 month')
nb = length(vec_flx)
vec_flx = format(vec_flx, format = "%Y%m%d")


### Passage du nom de table souhaite en majuscule (norme ORACLE) ----

tbl_nom_up = toupper(tbl_nom)


### Completion de la requete SQL ----

# A la premiere iteration on cree la table
# Aux suivantes on la complete avec INSERT INTO

sql1 = paste("CREATE TABLE",tbl_nom_up,"AS ")
sql2 = paste("INSERT INTO",tbl_nom_up," ")

# insertion des dates de debut et fin de la periode etudiee
sql = gsub(":DEBSOIAAAAMMJJ", format(debut_date, format = "%Y%m%d"), sql)
sql = gsub(":FINSOIAAAAMMJJ", format(fin_date, format = "%Y%m%d"), sql)



## Supression de la table si dejà existante ----

if (dbExistsTable(conn, tbl_nom_up)) {
  dbRemoveTable(conn, tbl_nom_up)
}

## Realisation de la boucle ----

cpt = 1    # compteur d'iteration


# boucle

for(i in vec_flx) {
  
  # insertion dis parcouru dans la requete 
  sql_complete = gsub(":FLUX", i, sql)
  
  
  # traitement des donnees avant 2013 avec les tables archives

  if (year(as.Date(i, format = "%Y%m%d")) < 2013) {
    
    if (cpt == 1){
      table_prs_last = "ER_PRS_F"
      table_pha_last = "ER_PHA_R"
    }
    
    year = year(as.Date(i, format = "%Y%m%d"))
    
    table_prs_new = paste("ER_PRS_F", year, sep = "_")
    table_pha_new = paste("ER_PHA_R", year, sep = "_")

    sql_complete = gsub(table_prs_last, table_prs_new, sql_complete)
    sql_complete = gsub(table_pha_last, table_pha_new, sql_complete)
    
  }
  
  # creation de table si premiere iteration, completion sinon
  if (cpt == 1) {

    sql_complete = paste(sql1, sql_complete) 
    
  }
  else {
    
    sql_complete = paste(sql2, sql_complete)
    
  }
  
  # execution de la requete et recuperation de la duree
  time = system.time(dbGetQuery(conn, sql_complete))

  # affichage
  cat("Iteration ", cpt, "/", nb, ": flux ", i, "termine en", time["elapsed"], "secondes\n")
  
  # incrementation du compteur
  cpt = cpt + 1
}


# 05 - Verification ----

## Presence de la table dans ORAUSER ----

# Verifier que le nom de la table apparait 

cat("\nTable existantes dans ORAUSER :")
cat("--> Verifier la présence de", tbl_nom_up)
dbListTables(conn)

# Nombre de lignes 
cat("\nNombre de ligne dans", tbl_nom_up, ":")
cat("--> Verifier la cohérence")
dbGetQuery(conn, paste("select count(*) as nrow from", tbl_nom_up)) 

# Aperçu de la table
cat("\nApercu de la table", tbl_nom_up, ":")
cat("--> Verifier la cohérence")
tbl(conn, tbl_nom_up) 








