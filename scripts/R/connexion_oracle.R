
################################################
#                                              #
#    Connexion à la base de données Oracle     #
#                                              #  
#	         	version 1 : 08/04/2025 				     #
#                                              #  
################################################

# 00 - Library ----

library(data.table)
library(dplyr)
library(lubridate)
library(dbplyr)
library(ROracle)


# 01 - Connexion ----

drv <- dbDriver("Oracle")
conn <- dbConnect(drv, dbname = "IPIAMPR2.WORLD")

Sys.setenv(TZ = "Europe/Paris") 
Sys.setenv(ORA_SDTZ = "Europe/Paris")

# 02 - Verification ----

dbListTables(conn)
