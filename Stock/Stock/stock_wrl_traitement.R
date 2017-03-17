#Chargement des librairies
library(readxl)
options(warn=-1)
options(digits = 20,scipen = 10)

#Chargement du repertoire de fichiers
source("O:/01-MAINTENANCE SI/ODS STOCK/stock_ods/bin/Stock/stock_depot.R")

#Detection Type, Entetes
source("O:/01-MAINTENANCE SI/ODS STOCK/stock_ods/bin/Stock/stock_type_fichier.R")

#Manipulation des fichiers stk
source("O:/01-MAINTENANCE SI/ODS STOCK/stock_ods/bin/Stock/stock_traitement_stk.R")

#Manipulation des fichiers exp
source("O:/01-MAINTENANCE SI/ODS STOCK/stock_ods/bin/Stock/stock_traitement_exp.R")

#Ecriture dans un repertoire des fichiers stk/exp
source("O:/01-MAINTENANCE SI/ODS STOCK/stock_ods/bin/Stock/stock_ecriture.R")
