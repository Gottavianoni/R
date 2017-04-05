#SCRIPT POUR CRITERES
Sys.setenv(http_proxy="*****")
Sys.setenv(https_proxy="*****")
Sys.setenv(encoding = "UTF-8")

if(!exists('liste_produit_conc_filtre')) liste_produit_conc_filtre <- NULL
library(rvest)
load("C:/Users/ottavig/Documents/beaute_test.RData")
for ( produit in liste_produit_conc) {
  try(html_prod <- html(produit))
  content <- html_prod %>% html_nodes("td.H5_textecourant td.H5_textecourant table tr td tr td")
  content <- content[1:28] %>% html_text()
  content <- gsub("Ã","a",content,fixed= T)
  content <- gsub("Ä","a",content,fixed= T)
  content <- gsub("Â","a",content,fixed= T)
  print(content)
  list_rech <- c("Hydrat","Anti-rid","Anti-","Lifta")
  flag <- "OK"
  for (mts in list_rech) {
    ind <- which(regexpr(mts,content,fixed= T) != -1)
    if (length(ind) == 0) ind <- 1
    content <- ifelse(is.na(content),"",content)
    print(content[ind])
    print(content[ind+1])
    if(content[ind+1] != "Oui")flag <- "KO"
    print(flag)
  }
  if (flag == "OK") liste_produit_conc_filtre <- c(liste_produit_conc_filtre,produit) 
  print(produit)
  print(paste("Il reste ",length(liste_produit_conc) - which(liste_produit_conc == produit)," produits a passer sur ",length(liste_produit_conc)," - ",flag,sep =""))
  save.image("C:/Users/ottavig/Documents/beaute_test.RData")
  Sys.sleep(2)
}
