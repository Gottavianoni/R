#Chargement des librairies
library(readxl)
options(warn=-1)
options(digits = 20,scipen = 10)
  
#Chargement du repertoire de fichiers
source("C:/Users/ottavig/Documents/Script_R/Stock/stock_depot.R")
  
#Detection Type, Entetes
source("C:/Users/ottavig/Documents/Script_R/Stock/stock_type_fichier.R")

#Manipulation des fichiers stk
source("C:/Users/ottavig/Documents/Script_R/Stock/stock_traitement_stk.R")

#Manipulation des fichiers exp
source("C:/Users/ottavig/Documents/Script_R/Stock/stock_traitement_exp.R")

#Ecriture dans un repertoire des fichiers stk/exp
source("C:/Users/ottavig/Documents/Script_R/Stock/stock_ecriture.R")
