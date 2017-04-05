#PHYTO
library(rvest)
library(RSelenium)
Sys.setenv(https_proxy = "*****")
Sys.setenv(http_proxy = "*****")
url <- "https://www.phyto.fr/recherche-de-magasins.html"
page <- read_html(url)
link <- page %>% html_nodes("div ol li a") %>% html_attr("href")

#SANS PROXY POUR UTILISER LE PHANTOMJS
Sys.setenv(https_proxy = "")
Sys.setenv(http_proxy = "")
phantom(pjs_cmd = "C:/Users/ottavig/Documents/phantomjs.exe",extra = "--proxy-auth=***** --proxy=*****")
remDr <- remoteDriver(browserName = "phantomjs")
remDr$open()

df <- NULL

for(dep in link) {
  remDr$navigate(dep)
  page <- read_html(enc2native(remDr$getPageSource()[[1]][1]))
  nom <- page %>% html_nodes("ol li strong.ng-binding") %>% html_text()
  lien <- page %>% html_nodes("p.btn-responsive-toolbar a") %>% html_attr("href")
  adresse <- page %>% html_nodes("div.col-xs-10 div.ng-binding.ng-scope") %>% html_text()
  tel <- page %>% html_nodes("p.btn-responsive-toolbar button span.ng-binding.ng-hide") %>% html_text()
  ind <- which(regexpr("[0-9]{5,5}",adresse) != -1)
  cp <- adresse[ind]
  voie <- adresse[ind-1]
  ville <- adresse[ind+1]
  
  
  if(!(length(nom) == length(voie) & length(voie) == length(cp) & length(cp) == length(ville))){
    ad2 <- page %>% html_nodes("div.col-xs-10") %>% html_text()
    ad2 <- gsub("\\s{2,}"," ",ad2)
    ad2 <- which(regexpr("[0-9]{5,5}",ad2) == -1)
    for(i in 1:length(ad2)) {
      cp <- c(cp[1:ad2[i]-1],"NR",cp[(ad2[i]):length(cp)])
      ville <- c(ville[1:ad2[i]-1],"NR",ville[(ad2[i]+1):length(cp)])
      voie <- c(voie[1:ad2[i]-1],"NR",voie[(ad2[i]+1):length(cp)])
    }
    adresse <- c(voie,cp,ville)
  }
  
  
  if(length(adresse)/3 != length(nom)){
  ind2 <- sort(c(ind-1,ind,ind+1))
  ind2 <- which(is.na(match(adresse,adresse[ind2])))
  adresse[ind2+1] <- paste(adresse[ind2],adresse[ind2+1],sep = " - ")
  rm(ind2)
  }
  

  res <- data.frame(cbind(nom,voie,ville,cp,tel,lien))
  df <- rbind(df,res) 
  print(dep)
  print(length(nom) == length(voie) & length(voie) == length(cp) & length(cp) == length(ville))
  Sys.sleep(2)
  
}


for (i in 2:4) {
  df[,i] <- gsub("Ã\u2030","E",df[,i])
  df[,i] <- gsub("Ã\u02C6","E",df[,i])
  df[,i] <- gsub("Ã\u0160","E",df[,i])
}

write.csv2(df,"C:/Users/ottavig/Desktop/phyto_pdv.csv",row.names=F)
