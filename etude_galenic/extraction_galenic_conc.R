#ETUDE GALENIC CONCURRENTIELLE
library(rvest)

liste_produit <- c("http://www.beaute-test.com/creme_teintee_hydra-lumiere_-_aquasublime_galenic.php"
                   ,"http://www.beaute-test.com/roll-on_illuminateur_anti-cernes_-_teint_ideal_vichy.php"
                   ,"http://www.beaute-test.com/creme_prodigieuse_dd_creme_nuxe.php"
                   ,"http://www.beaute-test.com/hydra-chrono__teinte_lierac.php"
                   ,"http://www.beaute-test.com/creme_teintee_hydratante_-_teint_eclat_prodigieux_nuxe.php"
                   ,"http://www.beaute-test.com/creme_teintee_minerale_-_teint_divin_caudalie.php")


for (produit in liste_produit) {
  source("C:/Users/ottavig/Documents/Script_R/beaute-test/beaute_test_1_produit.R")
  if (!exists("avis_prod_conc")) try(avis_prod_conc <- prodi,silent = F) else try(avis_prod_conc <- rbind(avis_prod_conc,prodi) ,silent =F )
  rm(prodi) 
  print(paste("Il reste ",length(liste_produit) - which(liste_produit == produit)," produits a passer sur ",length(liste_produit),sep =""))
  Sys.sleep(2)
}

avis <- avis_prod_conc
rm(avis_prod_conc)

for (col in c("an","ap","com")) {
  avis[,col] <- gsub("ã©","e",avis[,col],fixed = T)
  avis[,col] <- gsub("ã¨","e",avis[,col],fixed = T)
  avis[,col] <- gsub("ã¢","a",avis[,col],fixed = T) 
  avis[,col] <- gsub("Ã©","e",avis[,col],fixed = T)
  avis[,col] <- gsub("Ã¨","e",avis[,col],fixed = T)
  avis[,col] <- gsub("Ãª","i",avis[,col],fixed = T) 
  avis[,col] <- gsub("Ã¢","a",avis[,col],fixed = T)
  avis[,col] <- gsub("Ã§","c",avis[,col],fixed = T)  
  avis[,col] <- gsub("Ã®","i",avis[,col],fixed = T) 
  avis[,col] <- gsub("Ã»","a",avis[,col],fixed = T)
  avis[,col] <- gsub("(\n)", " ",avis[,col],perl = T)
  avis[,col] <- gsub("(\r)", " ",avis[,col],perl = T)
  avis[,col] <- gsub('(\")', " ",avis[,col],perl = T)
  avis[,col] <- gsub('(")', " ",avis[,col],perl = T)
  avis[,col] <- gsub("a*", "",avis[,col],fixed = T)
  avis[,col] <- gsub("/", " ",avis[,col],fixed = T)
  avis[,col] <- gsub("!", " ",avis[,col],fixed = T)
  avis[,col] <- gsub("'", " ",avis[,col],fixed = T)
  avis[,col] <- gsub("*", " ",avis[,col],fixed = T)
  avis[,col] <- gsub("-", " ",avis[,col],fixed = T)
  avis[,col] <- gsub(".", " ",avis[,col],fixed = T)
  avis[,col] <- gsub(",", " ",avis[,col],fixed = T)
  avis[,col] <- gsub("?", " ",avis[,col],fixed = T)
  avis[,col] <- gsub(")", " ",avis[,col],fixed = T)
  avis[,col] <- gsub("(", " ",avis[,col],fixed = T)  
  avis[,col] <- gsub("#", " ",avis[,col],fixed = T)
  avis[,col] <- gsub("&", " ",avis[,col],fixed = T)
  avis[,col] <- gsub("[", " ",avis[,col],fixed = T)    
  avis[,col] <- gsub("]", " ",avis[,col],fixed = T)
  avis[,col] <- gsub(":", " ",avis[,col],fixed = T)
  avis[,col] <- gsub("~", " ",avis[,col],fixed = T)
  avis[,col] <- gsub("=", " ",avis[,col],fixed = T)
  avis[,col] <- gsub("+", " ",avis[,col],fixed = T)
  avis[,col] <- gsub("^", " ",avis[,col],fixed = T)
  avis[,col] <- gsub("_", " ",avis[,col],fixed = T)
  avis[,col] <- gsub(">", " ",avis[,col],fixed = T)
  avis[,col] <- gsub("#NOM", "",avis[,col],fixed = T)
  avis[,col] <- gsub("./","",avis[,col],fixed = T)
  avis[,col] <- gsub("Ã","a",avis[,col],fixed = T)
  avis[,col] <- tolower(avis[,col])
}

#Generation de la base des consommateurs
liste_membre <- unique(as.character(avis$pseudo_avis))
source("C:/Users/ottavig/Documents/Script_R/beaute-test/beaute_test_membre_batch.R")
for (c in names(avis)) avis[,c] <- as.character(avis[,c])
for (c in names(pers_desc)) pers_desc[,c] <- as.character(pers_desc[,c])
avis <- merge(avis,pers_desc2,by.x = "pseudo_avis",by.y = "nom", all.x= T)

liste_membre <- avis[is.na(avis$sexe),"pseudo_avis"]

#recup des textes du web via google
library(rvest)
url <- "https://www.google.fr/search?hl=fr&num=100&q=neoptide+ducray+com'"
google_content <- html(url)
liens <- google_content %>% html_nodes("div ul li a") %>% html_attr("href") 
liens <- substr(liens,regexpr("\\:http\\:\\/\\/+|\\:https\\:\\/\\/+",liens),nchar(liens))
liens <- gsub("%2525C3%2525A8","e",liens)
liens <- gsub("%2525C3%2525A9","e",liens)
liens <- gsub("%25C3%25A9","e",liens)
liens <- gsub("%25C3%25A9","e",liens)
liens <- gsub("\\,","-",liens)
liens <- substr(liens,2,regexpr("%",liens)-1)
liens <- liens[liens != ""]
res <- NULL

for (link in liens) {
  content <- try(html(link) %>% html_nodes("body"))
  res <- c(res,content)
}

res <- gsub("\n","",res)
res <- gsub("\t","",res)
for (i in 1:50) res <- gsub("  "," ",res)
res <- gsub("[0-9]","",res)
res <- gsub("\\<","",res)
res <- gsub("\\>","",res)

writeLines(res,"C:/Users/ottavig/Desktop/gweb.txt")
