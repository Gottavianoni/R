library(rvest)
code_pays <- c("fr","es","de","it")
list_marque <- c("aderma","ducray","galenic","furterer","avene","elancyl","klorane","pierre+fabre+dermatologie")

df2 <- NULL

for(pays in code_pays){
  print(paste0("Extraction pour l'extension ",pays))
    for(marque in list_marque){
      print(paste0("Extraction de la marque ",marque))
url <- paste0("http://www.amazon.",pays,"/s/ref=nb_sb_noss?field-keywords=",marque)
page <- read_html(url)
nom <- page %>% html_nodes("div div div a[class='a-link-normal s-access-detail-page  a-text-normal'] h2") %>% html_text()
asin <- page %>% html_nodes("div div div a[class='a-link-normal s-access-detail-page  a-text-normal']") %>% html_attr("href") 
asin <- substr(asin,regexpr("dp",asin) + 3,regexpr("ref\\=",asin)-2)
lien_page <- page %>% html_nodes("div#pagn span.pagnLink a") %>% html_attr("href")
lien_page <- lien_page[1]
lien_page1 <- paste0("http://amazon.",pays,substr(lien_page,1,regexpr("pg_",lien_page) +2))
lien_page2 <- substr(lien_page,regexpr("\\?rh",lien_page),regexpr("page\\=",lien_page) +4)
lien_page3 <- substr(lien_page,regexpr("\\&keyword",lien_page),nchar(lien_page))
nb_page <- page %>% html_nodes("div#pagn span.pagnDisabled") %>% html_text()
if(length(nb_page) == 0) nb_page <- page %>% html_nodes("div#pagn span.pagnLink a") %>% html_text()
Sys.sleep(2)
if(length(nb_page) > 0) {

nb_page <- max(as.numeric(nb_page))
lien_page <- paste0(lien_page1,c(2:nb_page),lien_page2,c(2:nb_page),lien_page3)

if(length(nom) != 0 & length(asin) != 0){
df <- data.frame(cbind(nom,asin,marque,pays))
names(df) <- c("NOM","ASIN","MARQUE","PAYS")
}

for (link in lien_page){
page <- read_html(link)
nom <- page %>% html_nodes("div div div a[class='a-link-normal s-access-detail-page  a-text-normal'] h2") %>% html_text()
asin <- page %>% html_nodes("div div div a[class='a-link-normal s-access-detail-page  a-text-normal']") %>% html_attr("href") 
asin <- substr(asin,regexpr("dp",asin) + 3,regexpr("ref\\=",asin)-2)
if(length(nom) != 0 & length(asin) != 0){
res <- data.frame(cbind(nom,asin,marque,pays))
names(res) <- c("NOM","ASIN","MARQUE","PAYS")
df <- rbind(df,res)
}
print(paste0("page ",as.numeric(which(link == lien_page)) +1 ," sur ",nb_page))
Sys.sleep(2)

}#produit

df2 <- rbind(df2,df)
}#if 1 page

}#marque

}#pays

df <- df2
