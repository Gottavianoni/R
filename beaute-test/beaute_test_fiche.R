#Recup de la gamme a partir du produit
#Recup des avis moyende la gamme 
library(httr)
library(RCurl)
library(rvest)

#Recup information
#Adresse a recup
#----------------------------------------------
produit <- "http://www.beaute-test.com/doriance_solaire_et_anti-age_doriance.php" 
produit <- "http://www.beaute-test.com/triacneal_soin_avene.php"
produit <- "http://www.beaute-test.com/cellu_slim_elancyl.php"
#----------------------------------------------
conc1 <- "http://www.beaute-test.com/preparateur_solaire_-_phytobronz_arkopharma.php"
conc1 <- "http://www.beaute-test.com/effaclar_duo___la_roche_posay.php"
conc1 <- "http://www.beaute-test.com/traitement_ventre_et_hanches__somatoline_cosmetic.php"

#---------------------------------------------
conc2 <- "http://www.beaute-test.com/oenobiol_solaire_intensif__oenobiol.php"
conc2 <- "http://www.beaute-test.com/2_en_1_nettoyant_et_masque_-_visibly_clear_neutrogena.php"
#---------------------------------------------

prox <- use_proxy(url = "*****",port = "*****",username = "*****", password = "*****")
agent <- user_agent("Mozilla/5.0 (compatible; Googlebot/2.1; +http://www.google.com/bot.html)")

#trouver la gamme a partir du produit
html_produit_avis <- html(produit,prox,agent,timeout(10))
gamme <-  html_attr(html_produit_avis %>% html_nodes("a.lien_secondaire"),"href")
#FAIRE ATTENTION A L'INDICE DANS LA GAMME
gamme <-  paste("http://www.beaute-test.com/",gamme[2],sep="")

#page de la gamme
html_produit_avis <- html(gamme,prox,agent,timeout(10))
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
    html_produit_avis <- html(url,prox,agent,timeout(10))
    nb_avis <- html_produit_avis %>% html_nodes("div.barre_bulle-avis a") %>% html_text() 
    nb_avis <- as.numeric(gsub(" avis","",nb_avis))
    note_avis <- html_produit_avis %>% html_nodes("img")
    note_avis <- html_attr(note_avis,"onmousemove")
    note_avis <- note_avis[which(!is.na(note_avis))]
    note_avis <- as.numeric(substr(note_avis,14,16))
    
    if (i == 1) { 
      nt_avis <- note_avis
      n_avis <- nb_avis
    } else {
      nt_avis <- c(nt_avis,note_avis)
      n_avis <- c(n_avis,nb_avis)
    }
    if (i == nombre_pages)  { 
      note_avis <- nt_avis
      nb_avis <- n_avis
      moy <- round(sum(note_avis*nb_avis,na.rm = T)/sum(nb_avis,na.rm = T),1)
      tx <- (((3.9 - moy) /5) + 0.15 ) * 100  
    #rm(note_avis,n_avis,nt_avis,nb_avis,url,html_produit_avis)
    
    }
    
    Sys.sleep(2)
}

#Recup de tous les avis d'un produit

print(paste("La note moyenne de la gamme est : ",moy,sep=""))
print(paste("Le taux d avis defavorable est estimé à ", tx," %",sep=""))

source("~/Stats/beaute_test_avis_marque.R")
avis_pf <- prod
avis_pf <- read.csv2("~/Stats/Sources/avis_conso.csv")


produit <- conc1
source("~/Stats/beaute_test_1_produit.R")
avis_conc_1 <- prod 

produit <- conc2
source("~/Stats/beaute_test_1_produit.R")
avis_conc_2 <- prod 

liste_membre <- unique(c(avis_pf$pseudo_avis,avis_conc_1$pseudo_avis,avis_conc_2$pseudo_avis))
membre_acneal <- read.csv2("~/Stats/Sources/consmmateurs.csv")
test <- merge(rbind(avis_pf,avis_conc_1,avis_conc_2),membre_acneal,by.x = "pseudo_avis",by.y="nom",all.x = T)
test <- merge(x = test[,1:14],y = rbind(membre_acneal,pers_desc[,names(membre_acneal)]),by.x = "pseudo_avis", by.y = "nom", all.X = T)

source("~/Stats/batch.R")
rm(liste_membre)
source("~/Stats/beaute_test_membres.R")


avis <- read.csv2("C:/Documents and Settings/ottavig/Bureau/Elancyl_Slim_Concurrence/cellu_soma_comparaison.csv")
table(avis$nomproduit)

avis_conc <- avis[avis$nomproduit == "traitement_ventre_et_hanches__somatoline_cosmetic",]
avis_pf <- avis[avis$nomproduit == "cellu_slim_elancyl",]

#base de données complete avis, et utilisateurs
avis_pf$ens <- "pf"
avis_conc$ens <- "conc"
avis_cons <- rbind(avis_pf,avis_conc)

for (col in names(avis_cons)) avis_cons[,col] <- as.character(avis_cons[,col])
avis_cons <- merge(avis_cons,pers_desc,by.x= "pseudo_avis",by.y = "nom",all.x=T)

for (col in names(avis_pf)) avis_pf[,col] <- as.character(avis_pf[,col])
#création des ratios et moyennes PF
for (col in names(avis_pf)[5:12]){
moy <- mean(as.numeric(substr(avis_pf[,col],1,1)),na.rm = T)
assign(paste("pf",col,sep=""),moy)
avis_pf[,col] <- as.numeric(substr(avis_pf[,col],1,1)) 
rm(moy)
}
pfmoy = round(mean(c(pfpresentation,pfpenetration,pfqualprix,pfefct,pfeflt,pfodeur,pftexture),na.rm = T),1)
#rm(pfefficacite,pfpresentation,pfquallprix,pfpenetration,pfqualprix,pfefct,pfeflt,pfodeur,pftexture)
 

View(data.frame(cbind(c("presentation","penetration","qualprix","pfefct","pfeflt","odeur","texture"),round(c(pfpresentation,pfpenetration,pfqualprix,pfefct,pfeflt,pfodeur,pftexture),1))))

#choisir suivant ligne
avis_pf$ratio <- round((avis_pf$presentation + avis_pf$efficacite + avis_pf$texture + avis_pf$odeur + avis_pf$qualprix + avis_pf$penetration) / 6,1)
#---------------------

pfratio <- length(avis_pf[which(avis_pf$ratio > 2.5),1])/ length(avis_pf[,1]) * 100

#Concurrents
avis_conc <- rbind(avis_conc_1,avis_conc_2)
avis_conc <- avis_cons[avis_cons$ens == "conc",]
#création des ratios et moyennes conc
for (col in names(avis_conc)[5:13]){
  moy <- mean(as.numeric(substr(avis_conc[,col],1,1)),na.rm = T)
  assign(paste("conc",col,sep=""),moy)
  avis_conc[,col] <- as.numeric(substr(avis_conc[,col],1,1)) 
  rm(moy)
}
concmoy = round(mean(c(concpresentation,concpenetration,concqualprix,concefct,conceflt,concodeur,conctexture),na.rm = T),1)
rm(concefficacite,concpresentation,concquallprix,concpenetration,concqualprix,concefct,conceflt,concodeur,conctexture)

#choisir suivant ligne
avis_conc$ratio <- round((avis_conc$presentation + avis_conc$efficacite + avis_conc$texture + avis_conc$odeur + avis_conc$qualprix + avis_conc$penetration) / 6,1)
#---------------------

concratio <- length(avis_conc[which(avis_conc$ratio > 2.5),1])/ length(avis_conc[,1]) * 100
#

#Affichage total

print(paste("Moyenne Gamme = ",moy,sep=""))
print(paste("Moyenne Pf = ",pfmoy,sep=""))
print(paste("Moyenne Concurrence = ",concmoy,sep=""))
print(paste("Ratio Gamme = ",100-tx,sep=""))
print(paste("Ratio Pf = ",pfratio,sep=""))
print(paste("Ratio Concurrence = ",concratio,sep=""))

#------------------
#Profilage du client
#------------------

avis_pf <- avis
rm(avis)

table(avis_conc$age_avis)
View(table(avis_pf$age_avis))

table(avis_conc$peau)
View(table(avis_pf$peau))

table(avis_conc$desyd)
View(table(avis_pf$desyd))

table(avis_conc$age_avis)
View(table(avis_pf$age_avis))

table(avis_conc$cheveux)
View(table(avis_pf$cheveux))

table(substr(avis_conc$date_ins,7,8))
View(table(substr(avis_pf$date_ins,7,8)))


for (col in names(produit)) produit[,col] <- as.character(produit[,col])
#création des ratios et moyennes PF
for (col in names(produit)[5:13]){
  #moy <- mean(as.numeric(substr(produit[,col],1,1)),na.rm = T)
  #assign(paste("tot",col,sep=""),moy)
  produit[,col] <- as.numeric(substr(produit[,col],1,1)) 
  #rm(moy)
}

produit$date_avis <- substr(produit$date_avis,7,12)

prod1415 <- produit[produit$date_avis %in% c("2014","2015"),]
prod1013 <- produit[produit$date_avis %in% c("2010","2011","2012","2013"),]

for (c in names(prod1415)[5:11]) {
if(!exists("result")) result <- matrix(c(c,mean(prod1415[,c]),mean(prod1013[,c])),nrow = 1) else result <- rbind(result,c(col,mean(prod1415[,col]),mean(prod1013)))
}

totmoy = round(mean(c(totpresentation,totpenetration,totqualprix,totefct,toteflt,totodeur,tottexture),na.rm = T),1)
View(data.frame(cbind(c("presentation","penetration","qualprix","totefct","toteflt","odeur","texture",'efficacite',"qualprix2"),round(c(totpresentation,totpenetration,totqualprix,totefct,toteflt,totodeur,tottexture,totefficacite,totquallprix),1))))
rm(totefficacite,totpresentation,totquallprix,totpenetration,totqualprix,totefct,toteflt,totodeur,tottexture)

#choisir suivant ligne
produit$ratio <- round((produit$presentation + produit$eflt + produit$efct + produit$texture + produit$odeur + produit$qualprix + produit$penetration) / 7,1)
#---------------------
totratio <- length(produit[which(produit$ratio > 2.5),1])/ length(produit[,1]) * 100


for (col in names(avis_conc)[5:13]){
  moy <- mean(as.numeric(substr(avis_conc[,col],1,1)),na.rm = T)
  assign(paste("conc",col,sep=""),moy)
  avis_conc[,col] <- as.numeric(substr(avis_conc[,col],1,1)) 
  rm(moy)
}

concmoy = round(mean(c(concpresentation,concpenetration,concqualprix,concefct,conceflt,concodeur,conctexture),na.rm = T),1)
View(data.frame(cbind(c("presentation","penetration","qualprix","totefct","toteflt","odeur","texture",'efficacite',"qualprix2"),round(c(concpresentation,concpenetration,concqualprix,concefct,conceflt,concodeur,conctexture,concefficacite,concquallprix),1))))
rm(concefficacite,concpresentation,concquallprix,concpenetration,concqualprix,concefct,conceflt,concodeur,conctexture)



#choisir suivant ligne
avis_conc$ratio <- round((avis_conc$presentation + avis_conc$texture + avis_conc$qualprix + avis_conc$efficacite) / 4,1)
#---------------------

concratio <- length(avis_conc[which(avis_conc$ratio > 2.5),1])/ length(avis_conc[,1]) * 100
#


for (col in names(avis_pf)[5:13]){
  moy <- mean(as.numeric(substr(avis_pf[,col],1,1)),na.rm = T)
  assign(paste("pf",col,sep=""),moy)
  avis_pf[,col] <- as.numeric(substr(avis_pf[,col],1,1)) 
  rm(moy)
}

pfmoy = round(mean(c(pfpresentation,pfpenetration,pfqualprix,pfefct,pfeflt,pfodeur,pftexture),na.rm = T),1)

View(data.frame(cbind(c("presentation","penetration","qualprix","pfefct","pfeflt","odeur","texture","efficacite","qualprix2"),round(c(pfpresentation,pfpenetration,pfqualprix,pfefct,pfeflt,pfodeur,pftexture,pfefficacite,pfquallprix),1))))
rm(pfefficacite,pfpresentation,pfquallprix,pfpenetration,pfqualprix,pfefct,pfeflt,pfodeur,pftexture)
#choisir suivant ligne
avis_pf$ratio <- round((avis_pf$presentation + avis_pf$efficacite + avis_pf$quallprix) / 3,1)
#---------------------

pfratio <- length(avis_pf[which(avis_pf$ratio > 2.5),1])/ length(avis_pf[,1]) * 100

avis <- read.csv2("C:/Documents and Settings/ottavig/Bureau/Concurrence_P_Collet/Dexeryl/dexeryl_concurrents.csv")
avis <- read.csv2("C:/Documents and Settings/ottavig/Bureau/Concurrence_P_Collet/Arthrodont/arthrodont_conc_avis_utilisateurs.csv")
avis <- read.csv2("C:/Documents and Settings/ottavig/Bureau/Concurrence_P_Collet/Doriance/doriance_concurrents.csv")

for (col in names(avis)) avis[,col] <- as.character(avis[,col])
for (col in names(avis)) {
  avis[,col] <- gsub("Ã¨","e",avis[,col],fixed = T)
  avis[,col] <- gsub("Ã´","o",avis[,col],fixed = T)
  avis[,col] <- gsub("Ã©","e",avis[,col],fixed = T)
  avis[,col] <- gsub("Ã®","i",avis[,col],fixed = T)
  avis[,col] <- gsub("Ã§","c",avis[,col],fixed = T)
  avis[,col] <- gsub("Ãª","e",avis[,col],fixed = T)
  avis[,col] <- gsub("Ã¢", "a",avis[,col],fixed = T)
  avis[,col] <- gsub("Ã»", "u",avis[,col],fixed = T)
  avis[,col] <- gsub("(\n)", " ",avis[,col],perl = T)
  avis[,col] <- gsub("(\r)", " ",avis[,col],perl = T)
  avis[,col] <- gsub('(\")', " ",avis[,col],perl = T)
  avis[,col] <- gsub('(")', " ",avis[,col],perl = T)
  avis[,col] <- gsub("Ã", "a",avis[,col],fixed = T)
  avis[,col] <- gsub("Â", "",avis[,col],fixed = T)
}

write.csv2(avis,"C:/Documents and Settings/ottavig/Bureau/Concurrence_P_Collet/Dexeryl/dexeryl_concurrents.csv",row.names = F)
write.csv2(avis,"C:/Documents and Settings/ottavig/Bureau/Concurrence_P_Collet/Doriance/doriance_concurrents.csv",row.names = F)
write.csv2(avis,"C:/Documents and Settings/ottavig/Bureau/Concurrence_P_Collet/Arthrodont/arthrodont_conc_avis_utilisateurs.csv",row.names = F)

