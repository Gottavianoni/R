#SCRAPING BEAUTE TEST POUR UNE MARQUE RECUP DES TOUS LES AVIS

library(rvest)

if(exists("prodi_temp")){
prodi <- prodi_temp
rm(prodi_temp)
}

#FONCTION CRITERE et Efficacite
critere <- function(critere) {
  test1 <- html_produit_avis %>% html_nodes("td.notationfixe td.notecritere + td > img")
  test1 <- html_attr(test1,"onmousemove")
  test2 <- html_produit_avis %>% html_nodes("td.notationfixe td.notecritere") %>% html_text()
  test <- ifelse(test2 == critere,test1 ,NA)
  rm(test1,test2)
  test <- test[!is.na(test)]
  return(test)
  rm(test)
}

effi <- function(taille){
  test1 <- html_produit_avis %>% html_nodes("td.notationfixe td.notecritere + td > img")
  test1 <- html_attr(test1,"onmousemove")
  test2 <- html_produit_avis %>% html_nodes("td.notationfixe td.notecritere") %>% html_text()
  test <- ifelse(substr(test2,18,21) == taille,test1 ,NA)
  rm(test1,test2)
  test <- test[!is.na(test)]
  return(test)
  rm(test)
}

qprix <- function(taille){
  test1 <- html_produit_avis %>% html_nodes("td.notationfixe td.notecritere + td > img")
  test1 <- html_attr(test1,"onmousemove")
  test2 <- html_produit_avis %>% html_nodes("td.notationfixe td.notecritere") %>% html_text()
  test <- ifelse(substr(test2,20,24) == taille,test1 ,NA)
  rm(test1,test2)
  test <- test[!is.na(test)]
  return(test)
  rm(test)
}

cutmark <- function(text){
  text <- paste(substr(text,14,16),"/",substr(text,22,22),sep="")
  return(text)
}


url <- paste(produit,"?listeavis=1",sep="")
html_produit_avis <- html(url,encoding = "ISO8859-1")
nom_produit <- gsub(".php?listeavis=1","",url,fixed = T)
nom_produit <- gsub("http://www.beaute-test.com/","",nom_produit,fixed = T)
nombre_pages <- html_produit_avis %>% html_nodes("select.formclassic option[value='1']") %>% html_text()
nombre_pages <- as.numeric(substr(nombre_pages[1],regexpr("1/",nombre_pages[1],fixed = T) +2,nchar(nombre_pages[1])))
nombre_pages2 <- as.numeric(html_produit_avis %>% html_nodes("p.avispage a") %>% html_text())
nombre_pages2 <- max(nombre_pages2[!is.na(nombre_pages2)])
nombre_pages3 <- as.numeric(substr(html_produit_avis %>% html_nodes("p.avispage span") %>% html_text(),3,3))
nombre_pages3 <- max(c(nombre_pages3[!is.na(nombre_pages3)],nombre_pages2))
nombre_pages <- ifelse(is.na(nombre_pages),nombre_pages3,nombre_pages)
rm(nombre_pages2,nombre_pages3)

url = substr(url,1,(nchar(url) -1))

if(nombre_pages != "-Inf") {
if (!exists("index")) index <- 1


for(i in index:nombre_pages) {
    #Boucler sur tous les indices de pages jusqu'au nombre de pages
    #Pour une Page d'avis
    url_avis <- paste(url,i,sep="")
    html_produit_avis <- html(url_avis)
    date_avis <- html_produit_avis %>% html_nodes("div.notationavis td.details_titres strong") %>% html_text() 
    date_avis <- gsub("Commentaire du ","",date_avis,fixed = T)
    pseudo_avis <- html_produit_avis %>% html_nodes("div.notationavis td.details_titres a.pseudobt") %>% html_text() 
    #Age avec Regex
    age_avis <- html_produit_avis %>% html_nodes("div.notationavis td.details_titres") %>% html_text()
    age_avis <- substr(age_avis,regexpr("|",age_avis,fixed = T) + 2,100000)
    #Faire la boucle pour tous les produits
    #Recup de chacun des contenant
    #Prisentation
    presentation <- cutmark(critere("PrC)sentation"))
    #Texture
    texture <- cutmark(critere("Texture"))
    #Penetration
    penetration <- cutmark(critere("PC)nC)tration"))
    #effict
    efct <- cutmark(effi("cour"))
    #effilt
    eflt <- cutmark(effi("long"))
    #Odeur
    odeur <- cutmark(critere("Odeur"))
    #Rapport Qualite Prix
    qualprix <- cutmark(effi("/pri"))
    #Rapport Qualite Prix
    efficacite <- cutmark(critere("EfficacitC)"))
    #Qual/prix
    quallprix <- cutmark(qprix("prix"))
    
    #Commentaires,et Avis
    commentaire <- html_produit_avis %>% html_nodes("td.notationfixe + td p") %>% html_text()
    ap <- list()
    an <- list()
    com <- list()
    for (j in 1:length(commentaire)) {
      if (j%%3 == 1) ap <- c(ap,commentaire[j])
      if (j%%3 == 2) an <- c(an,commentaire[j])
      if (j%%3 == 0) com <- c(com,commentaire[j])
    }
    rm(commentaire)
    pg <- data.frame(cbind(nomproduit=rep(nom_produit,length(pseudo_avis)),pseudo_avis,date_avis,age_avis,presentation,texture,penetration,efct,eflt,odeur,qualprix,efficacite,quallprix,ap,an,com))
    rm(ap,an,com,odeur,qualprix,efct,eflt,texture,penetration,presentation,pseudo_avis,age_avis,date_avis,quallprix,efficacite)
    if (!exists("prodi")) prodi <- pg else prodi <- rbind(prodi,pg)
    if (i == nombre_pages ) rm(index,index_nom,index_ref)

 Sys.sleep(2)
 print(paste0("Page ",i," sur ",nombre_pages))
 save.image("C:/Users/ottavig/Documents/beaute_test.RData")
}
}
rm(i,j,critere,cutmark,effi,qprix,url_avis,url,pg,html_produit_avis,nombre_pages,nom_produit)