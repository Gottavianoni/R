#Beaute test Concurrents automatiques
#Recup de la gamme a partir du produit
#Recup des avis moyende la gamme 
library(rvest)

#Recup information
#Adresse a recup
#----------------------------------------------

#trouver la gamme a partir du produit
html_produit_avis <- html(produitc)
gamme <-  html_attr(html_produit_avis %>% html_nodes("a.lien_secondaire"),"href")
gamme <-  paste("http://www.beaute-test.com/",gamme[length(gamme)-1],sep="")
rm(produitc)

#page de la gamme
html_produit_avis <- html(gamme)
nombre_pages <- html_produit_avis %>% html_nodes("select.formclassic option[value='1']") %>% html_text()
nombre_pages <- as.numeric(gsub("1/","",nombre_pages[1],fixed = T))
nombre_pages2 <- as.numeric(html_produit_avis %>% html_nodes("p.avispage a") %>% html_text())
nombre_pages2 <- max(nombre_pages2[!is.na(nombre_pages2)])
nombre_pages3 <- as.numeric(substr(html_produit_avis %>% html_nodes("p.avispage span") %>% html_text(),3,3))
nombre_pages3 <- max(c(nombre_pages3[!is.na(nombre_pages3)],nombre_pages2))
nombre_pages <- ifelse(is.na(nombre_pages),nombre_pages3,nombre_pages)
rm(nombre_pages2,nombre_pages3)

for(i in 1:nombre_pages) {
  #Boucler sur tous les indices de pages jusqu'au nombre de pages
  #Pour une Page d'avis
  url = paste(gamme,"?page=",i,sep="")
  html_produit_avis <- html(url)
  html_conc1 <- html_produit_avis %>% html_nodes("table.tableau tr.td1 a[target=_blank]")
  html_conc2 <- html_produit_avis %>% html_nodes("table.tableau tr.td2 a[target=_blank]")
  produit_conc1 <- html_attr(html_conc1,"href")
  produit_conc2 <- html_attr(html_conc2,"href")
  produit_conc <- c(produit_conc1,produit_conc2)
  if (i == 1) liste_produit_conc <- produit_conc else liste_produit_conc <- c(liste_produit_conc,produit_conc) 
  Sys.sleep(2)
}

rm(produit,produit_conc,html_produit_avis,html_conc1,html_conc2,produit_conc2,produit_conc1,nombre_pages,liens_produits_conc,i)
liste_produit_conc <- paste("http://www.beaute-test.com/",liste_produit_conc,sep = "")
if(exists("index_nom")) liste_produit_conc <- liste_produit_conc[which(liste_produit_conc == index_nom):length(liste_produit_conc)]

#SCRIPT POUR CRITERES
liste_produit_conc_filtre <- NULL
for ( produit in liste_produit_conc) {
try(html_prod <- html(produit))
html_prod
content <- html_prod %>% html_nodes("td.H5_textecourant td.H5_textecourant table tr td tr td")
content <- content[1:28] %>% html_text()
content <- gsub("Ã¢","a",content,fixed= T)
list_rech <- c("Hydrat","Anti-ag","Anti-rid","Lifta")
flag <- "OK"
for (mts in list_rech) {
ind <- which(regexpr(mts,content,fixed= T) != -1)
if (length(ind) == 0) ind <- 1
content <- ifelse(is.na(content),"",content)
if( content[ind+1] != "Oui") flag <- "KO"
}
if (flag == "OK") liste_produit_conc_filtre <- c(liste_produit_conc_filtre,produit) 
print(produit)
print(paste("Il reste ",length(liste_produit_conc) - which(liste_produit_conc == produit)," produits a passer sur ",length(liste_produit_conc)," - ",flag,sep =""))
save.image("C:/Users/ottavig/Documents/beaute_test.RData")
Sys.sleep(2)
}

#Recup de tous les produits concurrents
for (produit in liste_produit_conc_filtre) {
try(source("C:/Users/ottavig/Documents/Script_R/beaute-test/beaute_test_1_produit.R"))
if (!exists("avis_prod_conc")) try(avis_prod_conc <- prodi,silent = F) else try(avis_prod_conc <- rbind(avis_prod_conc,prodi) ,silent =F )
rm(prodi) 
print(paste("Il reste ",length(liste_produit_conc_filtre) - which(liste_produit_conc_filtre == produit)," produits a passer sur ",length(liste_produit_conc_filtre),sep =""))
save.image("C:/Users/ottavig/Documents/beaute_test_filtre.RData")
}