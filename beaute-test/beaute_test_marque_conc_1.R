#Beaute test Concurrents automatiques
#Recup de la gamme a partir du produit
#Recup des avis moyende la gamme

library(httr)
library(RCurl)
library(rvest)

#Recup information
#Adresse a recup
#----------------------------------------------
marque <- "laboratoires_pierre_fabre"
#----------------------------------------------

prox <- use_proxy(url = "proxy1",port = 8080,username = "ottavig", password = "Pierre-Fabre01")
agent <- user_agent("Mozilla/5.0 (compatible; Googlebot/2.1; +http://www.google.com/bot.html)")

#Pour une marque
adr <- paste("http://www.beaute-test.com/",marque,".php",sep="")
html_marque <- html(adr,prox,agent,timeout(10)) 
marque_avis_int <- html_marque %>% html_nodes("div.barre_bulle-avis a")
marque_avis <- html_attr(marque_avis_int,"href")
marque_avis <- ifelse(substr(marque_avis,1,1) == "/",NA,marque_avis)
marque_avis <- marque_avis[!is.na(marque_avis)]
liens_produit <- paste("http://www.beaute-test.com/",marque_avis,sep="")
liens_produit <- gsub("?listeavis=1","",liens_produit,fixed = T)

rm(produit1c,marque_avis,marque_avis_int,html_marque,adr,marque)

for (produitc in liens_produit) {

  #trouver la gamme a partir du produit
  html_produit_avis <- html(produitc,prox,agent,timeout(10))
  gamme <-  html_attr(html_produit_avis %>% html_nodes("a.lien_secondaire"),"href")
  gamme <-  paste("http://www.beaute-test.com/",gamme[length(gamme)-1],sep="")
  
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
  
  if (nombre_pages != "-Inf") {
  
  for(i in 1:nombre_pages) {
    #Boucler sur tous les indices de pages jusqu'au nombre de pages
    #Pour une Page d'avis
    url = paste(gamme,"?page=",i,sep="")
    html_produit_avis <- html(url,prox,agent,timeout(10))
    html_conc1 <- html_produit_avis %>% html_nodes("table.tableau tr.td1 a[target=_blank]")
    html_conc2 <- html_produit_avis %>% html_nodes("table.tableau tr.td2 a[target=_blank]")
    produit_conc1 <- html_attr(html_conc1,"href")
    produit_conc2 <- html_attr(html_conc2,"href")
    produit_conc <- c(produit_conc1,produit_conc2)
    if (!exists("liste_produit_conc")) liste_produit_conc <- produit_conc else liste_produit_conc <- c(liste_produit_conc,produit_conc) 
    Sys.sleep(2)
  }
}
  rm(produit,produit_conc,html_produit_avis,html_conc1,html_conc2,produit_conc2,produit_conc1,liens_produits_conc,nombre_pages)
  if(exists("liste_totale_conc")) liste_totale_conc <- c(liste_totale_conc,liste_produit_conc) else liste_totale_conc <- liste_produit_conc 
}
rm(agent, gamme,i, liens_produit,liste_produit_conc, produitc,prox,url)

liste_totale_conc <- paste("http://www.beaute-test.com/",unique(liste_totale_conc),sep="")
