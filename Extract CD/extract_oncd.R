#ONCD EXTRACT 
library(rvest)
library(RSelenium)

nom <- gsub(" ","+",df1$NOM)
prenom <- gsub(" ","+",df1$PRENOM)
#cp <- substr(df1$L6_POST,1,5)

lien <- "http://www.ordre-chirurgiens-dentistes.fr/no-cache/grand-public/votre-praticien-en-un-clic.html?id=264&tx_oncdpraticien_pi1[nom]=&tx_oncdpraticien_pi1[prenom]=&tx_oncdpraticien_pi1[rue]=&tx_oncdpraticien_pi1[ville]=&tx_oncdpraticien_pi1[cp]=&tx_oncdpraticien_pi1[departement]=0&tx_oncdpraticien_pi1[recherche]=rechercher"
#lien <- paste0("http://www.ordre-chirurgiens-dentistes.fr/no-cache/grand-public/votre-praticien-en-un-clic.html?id=264&tx_oncdpraticien_pi1[nom]=",nom,"&tx_oncdpraticien_pi1[prenom]=",prenom,"&tx_oncdpraticien_pi1[rue]=&tx_oncdpraticien_pi1[ville]=&tx_oncdpraticien_pi1[cp]=",cp,"&tx_oncdpraticien_pi1[departement]=0&tx_oncdpraticien_pi1[recherche]=rechercher")
lien <- paste0("http://www.ordre-chirurgiens-dentistes.fr/no-cache/grand-public/votre-praticien-en-un-clic.html?id=264&tx_oncdpraticien_pi1[nom]=",nom,"&tx_oncdpraticien_pi1[prenom]=",prenom,"&tx_oncdpraticien_pi1[rue]=&tx_oncdpraticien_pi1[ville]=&tx_oncdpraticien_pi1[cp]=&tx_oncdpraticien_pi1[departement]=0&tx_oncdpraticien_pi1[recherche]=rechercher")
liens <- lien

lien <- "http://www.ordre-chirurgiens-dentistes.fr/no-cache/grand-public/votre-praticien-en-un-clic.html?tx_oncdpraticien_pi1[departement]=0&tx_oncdpraticien_pi1[recherche]=rechercher"
page <- html(lien)
nb <- page %>% html_nodes("div.search_result_header h3 span") %>% html_text()
nb <- substr(nb,1,regexpr("r",nb,fixed = T)-2)
nb <- round(as.numeric(nb)/20,0)
lien <- paste0(substr(lien,1,nchar(lien)-1),"&tx_oncdpraticien_pi1[pointer]=",c(0:nb))

pages <-NULL
for ( link in lien) {
 page <- html(link)
 med <-  page %>% html_nodes("div.result_list ul li h4 a") %>% html_attr("href")
 med <- paste0("http://www.ordre-chirurgiens-dentistes.fr/",med)
 pages <- c(pages,med)
 Sys.sleep(3)
 print(paste0(which(link == lien)," sur ",length(lien)))
}   

lien <- liens[5]

pages <- NULL
for (lien in liens) {
  page <- html(lien)
  page <- page %>% html_nodes("div.result_list ul li h4 a") %>% html_attr("href")
  page <- paste0("http://www.ordre-chirurgiens-dentistes.fr/",page)
  pages <- c(pages,page)
  Sys.sleep(3)
  print(paste0(which(lien == liens)," sur ",length(liens)))
}

ref_cd <- NULL
for(link in pages) {
chir <- html(link)  
nom <- chir %>% html_nodes("div h2 span span") %>% html_text()

adr <- chir %>% html_nodes("div.block_gray_int ul li span") %>% html_text()
if(length(adr) == 0) adr <- "NR"
if(adr == "") adr <- "NR"


tel <- chir %>% html_nodes("div.block_gray_int ul li") %>% html_text()
tel <- tel[which(regexpr("Tel :",tel,fixed=T) != -1)]
tel <- substr(tel,regexpr(":",tel)+2,nchar(tel))
if(length(tel) == 0) tel <- "NR"

rpps <- chir %>% html_nodes("div.block_gray_int ul li") %>% html_text()
rpps <- rpps[which(regexpr("RPPS :",rpps,fixed=T) != -1)]
rpps <- substr(rpps,regexpr(":",rpps)+2,nchar(rpps))
if(length(rpps) == 0) rpps <- "NR"

temp <- data.frame(t(c(nom,adr,tel,rpps)))
names(temp) <- c("nom","adr","tel","rpps")
ref_cd <- rbind(ref_cd,temp)
names(ref_cd) <- c("nom","adr","tel","rpps")

Sys.sleep(2)
print(nom)
save.image("C:/Users/ottavig/Documents/oncd.RData")
rm(nom,adr,tel,rpps,temp,chir)
}

df1 <- merge(df1,ref_cd,by.x = "RPPS",by.y = "rpps",all.x =T )

write.csv2(ref_cd, "C:/Users/ottavig/Desktop/extract_oncd.csv",row.names=FALSE)
load("C:/Users/ottavig/Documents/oncd.RData")
write.csv2(ref_cd, "C:/Users/ottavig/Desktop/extract_oncd.csv")
library(RSelenium)
library(RSQLServer)