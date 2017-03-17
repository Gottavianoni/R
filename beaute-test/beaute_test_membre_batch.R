#TEST
#Epurer les r�sultats
load("C:/Users/ottavig/Documents/beaute_test_membres.RData")
library(rvest)
Sys.setenv(http_proxy="http://ottavig:Pierre-Fabre02@proxy1:8080")
Sys.setenv(https_proxy="http://ottavig:Pierre-Fabre02@proxy1:8080")
Sys.setenv(encoding = "UTF-8")

#recup des membres

for (membre in liste_membre){
  pers <- paste("http://www.beaute-test.com/service/profil.php?membre=",membre,sep="")
  html_personne <- html(pers)
  nom <- html_personne %>% html_nodes("div.profil_gauche_titre") %>% html_text()
  content <- html_personne %>% html_nodes("div.profil_gauche_content") %>% html_text()
  content <- content[1:2]
  if (length(nom) != 0 | !is.na(content[1]) | !is.na(content[2]) ) { 
    pseudo <- data.frame(cbind(nom,desc = content[1],caract = content[2]))
    if (!exists("pers_desc")) pers_desc <- pseudo else pers_desc <- rbind(pers_desc,pseudo)
  }
  Sys.sleep(2)
  print(paste0("Il reste ",length(liste_membre) - which(membre == liste_membre)," sur ", length(liste_membre) ))
  save.image("C:/Users/ottavig/Documents/beaute_test_membres.RData")
}

pers_desc$sexe <- substr(pers_desc$desc,1,1)
pers_desc$date_ins <- substr(pers_desc$desc,22,30)
pers_desc$dep <- substr(pers_desc$desc,as.numeric(gregexpr( "(",pers_desc$desc,fixed = T)) + 1, as.numeric(gregexpr( ")",pers_desc$desc,fixed = T)) -1 )
pers_desc$peau<- substr(pers_desc$caract,as.numeric(gregexpr( "Type de peau : ",pers_desc$caract,fixed = T)) + 15,as.numeric(gregexpr( "shydratation",pers_desc$caract,fixed = T)) - 4)
pers_desc$desyd<- substr(pers_desc$caract,as.numeric(gregexpr( "hydratation : ",pers_desc$caract,fixed = T)) + 14,as.numeric(gregexpr( "Type de cheveux",pers_desc$caract,fixed = T)) - 1)
pers_desc$cheveux <- substr(pers_desc$caract,as.numeric(gregexpr( "Type de cheveux : ",pers_desc$caract,fixed = T)) + 18,as.numeric(gregexpr( "Type de cheveux : ",pers_desc$caract,fixed = T)) + 50)
pers_desc$dep <- gsub("é","e",pers_desc$dep)
pers_desc$dep <- gsub("ô","o",pers_desc$dep)
pers_desc$dep <- gsub("è","e",pers_desc$dep)
pers_desc$dep <- toupper(pers_desc$dep)
pers_desc <- pers_desc[,c(1,4:9)]

pers_desc$peau <- gsub("é","e",pers_desc$peau)
pers_desc$peau <- gsub("ô","o",pers_desc$peau)
pers_desc$peau <- gsub("è","e",pers_desc$peau)

pers_desc$cheveux <- gsub("é","e",pers_desc$cheveux)
pers_desc$cheveux <- gsub("ô","o",pers_desc$cheveux)
pers_desc$cheveux <- gsub("è","e",pers_desc$cheveux)

pers_desc$desyd <- gsub("é","e",pers_desc$desyd)
pers_desc$desyd <- gsub("ô","o",pers_desc$desyd)
pers_desc$desyd <- gsub("è","e",pers_desc$desyd)

pers_desc$desyd <- gsub("é","e",pers_desc$desyd)
pers_desc$desyd <- gsub("ô","o",pers_desc$desyd)
pers_desc$desyd <- gsub("è","e",pers_desc$desyd)

liste_membre <- gsub("é","e",liste_membre)
liste_membre <- gsub("ô","o",liste_membre)
liste_membre <- gsub("è","e",liste_membre)
liste_membre <- gsub("�","a",liste_membre)

save.image("C:/Users/ottavig/Documents/beaute_test_membres.RData")
rm(prod,produit,prox,pseudo,pers,membre,html_personne)
