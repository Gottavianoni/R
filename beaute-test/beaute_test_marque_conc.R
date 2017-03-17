#Recup de toute la concurrence
#SCRAPING BEAUTE TEST POUR UNE MARQUE RECUP DES TOUS LES AVIS

#if(!exists("liste_totale_conc")) source("~/stats/beaute_test_marque_conc_1.R")
#if (exists("index_nom")) liste_totale_conc <- liste_totale_conc[which(liste_totale_conc == index_nom):length(liste_totale_conc)]
load("S:/Documents/conc_dex.RData")
for (produit in liste_produit_conc) {
source("~/Stats/beaute_test_1_produit.R")
if (!exists("tot_conc")) tot_conc <- prodi else try(tot_conc <- rbind(tot_conc,prodi),silent = T)
rm(prodi)
}
