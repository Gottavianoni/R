#Nouvelle Session
#SCRAPING BEAUTE TEST POUR UNE MARQUE RECUP DES TOUS LES AVIS
library(rvest)
#------------------------------------
#LA MARQUE
marque <- "galenic"
#------------------------------------


#Pour une marque
adr <- paste("http://www.beaute-test.com/",marque,".php",sep="")
html_marque <- read_html(adr) 
marque_avis_int <- html_marque %>% html_nodes("div.barre_bulle-avis a")
marque_avis <- html_attr(marque_avis_int,"href")
marque_avis <- ifelse(substr(marque_avis,1,1) == "/",NA,marque_avis)
marque_avis <- marque_avis[!is.na(marque_avis)]
liens_avis_marque <- paste("http://www.beaute-test.com/",marque_avis,sep="")
liens_avis_marque <- gsub("?listeavis=1","",liens_avis_marque,fixed = T)
rm(marque_avis,marque_avis_int)

#liens_avis_marque <- c("http://www.beaute-test.com/creme_-__tolerance_extreme_avene.php","http://www.beaute-test.com/lait_nettoyant_-_tolerance_extreme_avene.php")

for (produit in liens_avis_marque) {
  source("~/Script_R/beaute-test/beaute_test_1_produit.R")
  if (!exists("avis_prod_conc")) avis_prod_conc <- prodi else avis_prod_conc <- rbind(avis_prod_conc,prodi) 
  rm(prodi)
}

df <- avis_prod_conc

for( c  in names(df))df[,c] <- as.character(df[,c])
for( c  in names(df)[c(5:13)]) {
  df[,c] <- gsub("\\.[0]\\/\\(","",df[,c])
}
for( c  in names(df)[c(1,2,14:16)]) {
  #NOM PRODUIT
  df[,c] <- gsub("\\s{2,}"," ",df[,c])
  df[,c] <- gsub("\n","",df[,c])
  df[,c] <- gsub("\\n","",df[,c])
  df[,c] <- gsub("\t","",df[,c])
  df[,c] <- gsub("\\t","",df[,c])
  df[,c] <- gsub("\r","",df[,c])
  df[,c] <- gsub("\\r","",df[,c])
  df[,c] <- gsub("è","e",df[,c])
  df[,c] <- gsub("é","e",df[,c])
  df[,c] <- gsub("ê","e",df[,c])
  df[,c] <- gsub("ë","e",df[,c])
  df[,c] <- gsub("à","a",df[,c])
  df[,c] <- gsub("ç","c",df[,c])
  df[,c] <- gsub("û","u",df[,c])
  df[,c] <- gsub("ü","u",df[,c])
  df[,c] <- gsub("î","i",df[,c])
  df[,c] <- gsub("ï","i",df[,c])
  df[,c] <- gsub("ô","o",df[,c])
  df[,c] <- gsub("ö","o",df[,c])
  df[,c] <- gsub("â","a",df[,c])
  df[,c] <- gsub("\\-"," ",df[,c])
  df[,c] <- gsub("\\_"," ",df[,c])
  df[,c] <- gsub("\\°","",df[,c])
  df[,c] <- gsub("\\|","",df[,c])
  df[,c] <- gsub("È","E",df[,c],fixed = T)
  df[,c] <- gsub("É","E",df[,c],fixed = T)
  df[,c] <- gsub("Ê","E",df[,c],fixed = T)
  df[,c] <- gsub("À","A",df[,c],fixed = T)
  df[,c] <- gsub("Â","A",df[,c],fixed = T)
  df[,c] <- gsub("Û","U",df[,c],fixed = T)
  df[,c] <- gsub("Ê","E",df[,c],fixed = T)
  
  df[,c] <- gsub("Ã¨","e",df[,c],fixed = T)
  df[,c] <- gsub("Â©","e",df[,c],fixed = T)
  df[,c] <- gsub("Ã©","e",df[,c],fixed = T)
  df[,c] <- gsub("Â\u2030","e",df[,c],fixed = T)
  df[,c] <- gsub("Ã\u2030","e",df[,c],fixed = T)
  df[,c] <- gsub("Ãˆ","e",df[,c],fixed = T)
  df[,c] <- gsub("Ã¨","e",df[,c],fixed = T)
  df[,c] <- gsub("Ã¯","i",df[,c],fixed = T)
  df[,c] <- gsub("Ã¯","i",df[,c],fixed = T)
  df[,c] <- gsub("Ã«","e",df[,c],fixed = T)
  df[,c] <- gsub("Ã©","e",df[,c],fixed = T)
  df[,c] <- gsub("Ã»","u",df[,c],fixed = T)
  df[,c] <- gsub("Ãª","e",df[,c],fixed = T)
  df[,c] <- gsub("Ã¨","e",df[,c],fixed = T)
  df[,c] <- gsub("Ã¢","a",df[,c],fixed = T)
  df[,c] <- gsub("Ã´","o",df[,c],fixed = T)
  df[,c] <- gsub("Ã§","c",df[,c],fixed = T)
  df[,c] <- gsub("Ã®","i",df[,c],fixed = T)
  df[,c] <- gsub("ÃŠ","e",df[,c],fixed = T)
  df[,c] <- gsub("Ã¤","a",df[,c],fixed = T)
  df[,c] <- gsub("Ã\u20AC","a",df[,c],fixed=T)
  df[,c] <- gsub("Ã\u203A","U",df[,c],fixed = T)
  df[,c] <- gsub("AÃQ","AIQ",df[,c],fixed = T)
  df[,c] <- gsub("\\sÃ\\s"," a ",df[,c])
  df[,c] <- gsub("Ã³","o",df[,c],fixed = T)
  df[,c] <- gsub("Ãº","o",df[,c],fixed = T)
  df[,c] <- gsub("Ã\u0192A¨","e",df[,c])
  df[,c] <- gsub("Ã\u0192A©","e",df[,c])
  df[,c] <- gsub("Ã\u0191A¨","e",df[,c])
  df[,c] <- gsub("Ã\u0191A©","e",df[,c])
  df[,c] <- gsub("Ã\u0192","a",df[,c])
  df[,c] <- gsub("Ã¡","e",df[,c])
  
  df[,c] <- gsub("\\.","",df[,c])
  df[,c] <- gsub("\\-"," ",df[,c])
  df[,c] <- gsub("\\'"," ",df[,c])
  df[,c] <- gsub("A DERMA","ADERMA",df[,c],fixed= T)
  
  df[,c] <- gsub("\\(","",df[,c])
  df[,c] <- gsub("\\)","",df[,c])
  df[,c] <- gsub("\\[","",df[,c])
  df[,c] <- gsub("\\]","",df[,c])
  
  df[,c] <- gsub("\\s{2,}"," ",df[,c])
}

write.csv2(df,"C:/users/ottavig/Desktop/beaute_test_caudalie_full.csv",row.names = F)
