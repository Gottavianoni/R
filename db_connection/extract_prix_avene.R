library	(rvest)
date <- as.character(Sys.Date())
heure <- paste0(as.character(substr(Sys.time(),12,13)),":00")

#PARADRUGS
site <- "paradrugs"
marque <- "avene"

df <- NULL
for (i in 1:4)  {
  if (i ==1) url <- paste0("http://www.paradrugs.com/search/show/48?q=",marque) else url <- paste0("http://www.paradrugs.com/search/page/",i,"/show/48?q=",marque)
  page <- html(url)
  name <- page %>% html_nodes("h2.product-name a") %>% html_attr("title")
  prix <- page %>% html_nodes("div.review-price span.price") %>% html_text()
  prix <- substr(prix,1,nchar(prix)-5)
  res <- data.frame(cbind(name,prix,site,date,heure))
  name <- gsub("Ã‰","E",name,fixed = T)
  df <- rbind(df,res)
  Sys.sleep(2)
  rm(res,prix,name,page,url)
}


#EASYPARAPHARMACIE

site <- "easyparapharmacie"

for (i in 1:4) {
  url <- paste0("http://www.easyparapharmacie.com/catalogsearch/result/index/?cat=0&limit=48&manufacturer=585&p=",i,"&q=",marque)
  page <- html(url) 
  name <- page %>% html_nodes("ul div.regular a.product-name") %>% html_text()
  name <- gsub("Ãˆ","E",name,fixed = T)
  name <- gsub("Ã‰","E",name,fixed = T)
  prix <- page %>% html_nodes("ul div.regular span.regular-price span.price, p.special-price span.price") %>% html_text()
  ind <- which(regexpr("\n",prix) != -1)
  if(length(ind) > 0){
  ind <- ind[seq(1,length(ind),by=2)]
  ind <- sort(c(ind,which(regexpr("\n",prix) == -1)),decreasing = F)
  prix <- prix[ind]
  }
  res <- data.frame(cbind(name,prix,site,date,heure))
  df <- rbind(df,res)
  Sys.sleep(2)
  rm(res,prix,name,page,url)
}

#PHARMACIE POLYGONE

site <- "pharmacie du polygone"

for (i in 1:3){
  url <- paste0("http://www.pharmaciepolygone.com/index.php?controller=search&search_query=",marque,"&orderby=position&orderway=desc&n=60&p=",i)
  page <- html(url)
  name <- page %>% html_nodes("div.right-block h5 a") %>% html_text()
  prix <- page %>% html_nodes("span[class='price product-price']") %>% html_text()
  prix<- prix[seq(1,length(name)*2,by=2)]
  res <- data.frame(cbind(name,prix,site,date,heure))
  
  df <- rbind(df,res)
  Sys.sleep(2)
  rm(res,prix,name,page,url)
}

#PHARMACIE MONGE

site <- "pharmacie monge"

for (i in 1:2){
  url <- paste0("http://www.pharmacie-monge.fr/index.php/catalogsearch/result/index/?limit=90&p=",i,"&q=",marque)
  page <- html(url)
  name <- page %>% html_nodes("h2.product-name a") %>% html_text()
  name <- gsub("\n","",name)
  name <- gsub("\t","",name)
  brand <- page %>% html_nodes("h3.marque a") %>% html_text()
  brand <- gsub("Ã¨","e",brand)
  name <- paste(brand,name)
  prix <- page %>% html_nodes("div.price-box span.regular-price") %>% html_text()
  prix <- page %>% html_nodes("div.price-box p.special-price span.price,div.price-box span.regular-price span.price") %>% html_text()
  prix <- substr(prix,regexpr("[0-9]",prix),regexpr("[0-9]",prix) +11)
  res <- data.frame(cbind(name,prix,site,date,heure))
  df <- rbind(df,res)
  Sys.sleep(2)
  rm(res,prix,name,page,url)
}


#PARATAMTAM

site <- "paratamtam"

for (i in 1:6){
  url <- paste0("http://paratamtam.com/manufacturer.php?id_manufacturer=21&noredirect=1&p=",i)
  page <- html(url)
  name <- page %>% html_nodes("div.special_slider_home h5[style='display:block;'] a") %>% html_attr("title")
  name <- name[-c(1:10)]
  prix <- page %>% html_nodes("div.display-prices span.price") %>% html_text()
  prix <- prix[1:28]
  res <- data.frame(cbind(name,prix,site,date,heure))
  df <- rbind(df,res)
  Sys.sleep(2)
  rm(res,prix,name,page,url)
}


#COMPTOIR SANTE
#RSELENIUM OBLIGE
site  <- "comptoir sante"

for (i in 1:10) {
  url <- paste0("http://www.comptoirsante.com/marques/avene.html?p=",i)
  page <- html(url)
  name <- page %>% html_nodes("div.category-products ul div.product-info a.product-name") %>% html_text()
  brand <- page %>% html_nodes("div.category-products ul div.product-desc a") %>% html_text()
  name <- paste(brand,name)
  prix_ent <- page %>% html_nodes("div.category-products ul span.price-from + div span[class='regular-price regular-without-promo'] span.partieEntiere,p.special-price span.partieEntiere") %>% html_text()
  prix_ent <- prix_ent[seq(1,length(prix_ent),by=2)]
  prix_dec <- page %>% html_nodes("div.category-products ul span.price-from + div span[class='regular-price regular-without-promo'] span.partieDecimale,p.special-price span.partieDecimale") %>% html_text()
  prix_dec <- substr(prix_dec[seq(1,length(prix_dec),by=2)],5,50)
  prix <- paste(prix_ent,prix_dec,sep=",")
  name <- gsub("Ãƒ","",name)
  name <- gsub("Ãˆ","e",name)
  name <- gsub("Ã©","e",name)
  name <- gsub("Ã¨","e",name)
  name <- gsub("Â©","e",name) 
  name <- gsub("Â¨","e",name)  
  res <- data.frame(cbind(name,prix,site,date,heure))
  res <- res[1:24,]
  Sys.sleep(2)
  df <- rbind(df,res)
}

df <- df[!is.na(df$name),]
#PRADO MERMOZ

site <- "prado mermoz"

url <- paste0("http://www.parapharmacie-et-medicament.com/web/p/9/0/0/QUERY=",marque,"/npp=MAX")
page <- html(url)
name <- page %>% html_nodes("div.caps_name_area h2 a") %>% html_attr("title")
prix <- page %>% html_nodes("div.caps_price_area div.price") %>% html_text()
res <- data.frame(cbind(name,prix,site,date,heure))
res$name <- gsub("Ãˆ","E",res$name)
df <- rbind(df,res)
rm(res,prix,name,page,url)


#POWERSANTE

site <- "powersante"

for (i in 1:5){
  url <- paste0("http://www.powersante.com/",marque,"?p=",i)
  page <- html(url)
  brand <- page %>% html_nodes("div.category-products li.item h2.product-brand a") %>% html_text()
  name <- page %>% html_nodes("div.category-products li.item h2.product-name a") %>% html_text()
  name <- paste(brand,name)
  prix <- page %>% html_nodes("div.category-products li.item span.regular-price, p.special-price span.price") %>% html_text()
  if(length(which(nchar(prix) < 30)) > 0 ) prix <- prix[-which(nchar(prix) < 30)]
  prix <- substr(prix,regexpr("[0-9]",prix),regexpr("[0-9]",prix)+11)
  res <- data.frame(cbind(name,prix,site,date,heure))
  df <- rbind(df,res)
  Sys.sleep(2)
  rm(res,prix,name,page,url,brand)
}

#PHARMARKET

site <- "pharmarket"

for (i in 1:6){
  url <- paste0("https://www.pharmarket.com/recherche?term=",marque,"&page=",i)
  page <- html(url)
  brand <- page %>% html_nodes("div.listProducts div.name span.brandName") %>% html_text()
  brand <- gsub("Ãˆ","E",brand)
  brand <- gsub("Ã©","E",brand)
  brand <- gsub("Ã¨","E",brand)
  name <-  page %>% html_nodes("div.listProducts div.buttonBookmark + a img") %>% html_attr("alt")
  name <- gsub("Ãˆ","e",name)
  name <- gsub("Ã©","e",name)
  name <- gsub("Ã¨","e",name)
  name <-paste(brand,name)
  prix <- page %>% html_nodes("div.listProducts div.action span span.new") %>% html_text()
  res <- data.frame(cbind(name,prix,site,date,heure))
  res <- res[substr(res$name,1,5) == "AVENE",]
  df <- rbind(df,res)
  Sys.sleep(2)
  rm(res,prix,name,page,url)
}

#PHARMAMARKET

site <- "pharmamarket"

for (i in 1:5){
  url <- paste0("http://www.pharmamarket.be/be_fr/catalogsearch/result/index/?limit=96&mode=grid&q=",marque,"&p=",i)
  page <- html(url)
  name <- page %>% html_nodes("div.category-products ul h2.product-name") %>% html_text()
  name <- gsub("Ã¨","e",name)
  prix <- page %>% html_nodes("div.category-products ul div[class='price-box clearfix'] p.special-price span.price") %>% html_text()
  prix <- substr(prix,regexpr("[0-9]",prix),regexpr("[0-9]",prix)+11)
  res <- data.frame(cbind(name,prix,site,date,heure))
  res <- res[substr(res$name,1,5) == "Avene",]
  df <- rbind(df,res)
  Sys.sleep(2)
  rm(res,prix,name,page,url)
}

#PARAPHARMAZEN

site <- "parapharmazen"

for (i in 1:2){
  url <- paste0("http://www.parapharmacie-en-ligne.com/recherche-base?search_query=",marque,"&n=280&p=",i)
  page <- html(url)
  name <- page %>% html_nodes("div.content_product h3 a") %>% html_attr("title")
  prix <- page %>% html_nodes("div.content_product span[class='prix price']") %>% html_text()
  prix <- substr(prix,regexpr("[0-9]",prix),regexpr("[0-9]",prix)+11)
  res <- data.frame(cbind(name,prix,site,date,heure))
  df <- rbind(df,res)
  Sys.sleep(2)
}

#HYPER PARA

site <- "hyper para"

for (i in 1:2){
  url <- paste0("http://www.hyperparapharmacie.com/marques/",marque,"?dir=desc&limit=100&no_cache=true&p=",i)
  page <- html(url)
  name <- page %>% html_nodes("div.category-products p.name a") %>% html_attr("title")
  name <- substr(name,regexpr("[A-Z]",name),nchar(name))
  brand <- page %>% html_nodes("div.category-products p.marque a") %>% html_text()
  brand <- substr(brand,regexpr("[A-Z]",brand),nchar(brand))
  brand <- gsub("Ã¨","e",brand)
  name <- paste(brand,name)
  prix <- page %>% html_nodes("li.item div.price-box span.regular-price,p.special-price span.price") %>% html_text()
  prix <- substr(prix,regexpr("[0-9]",prix),regexpr("[0-9]",prix)+11)
  res <- data.frame(cbind(name,prix,site,date,heure))
  df <- rbind(df,res)
  Sys.sleep(2)
}


#1001 PHARMACIES

site <- "1001 pharmacies"

for(i in seq(0,520,by=24)) {
  url <- paste0("http://www.1001pharmacies.com/s/",marque,"?offset=",i)
  page <- html(url)
  name <- page %>% html_nodes("h2.product_title a") %>% html_text()
  name <- substr(name,regexpr("[A-Z]",name),nchar(name))
  brand <- page %>% html_nodes("h4[class='lab-name purple'] a") %>% html_text()
  brand <- gsub("Ã¨","e",brand)
  name <- gsub("Ãˆ","e",name)
  name <- paste(brand,name)
  prix <- page %>% html_nodes("span.product_price") %>% html_text()
  res <- data.frame(cbind(name,prix,site,date,heure))
  res <- res[substr(res$name,1,5) == "Avene",]
  df <- rbind(df,res)
  Sys.sleep(2)
}

#SANTE DISCOUNT

site <- "sante discount"

for(i in 1:2) {
  url <- paste0("https://www.santediscount.com/catalogsearch/result/index/?limit=120&q=",marque,"&p=",i)
  page <- html(url)
  name <- page %>% html_nodes("li.product h2.product-name a") %>% html_text()
  name <- substr(name,regexpr("[A-Z]",name),nchar(name))
  prix <- page %>% html_nodes("div.price-box span.regular-price span.price") %>% html_text()
  res <- data.frame(cbind(name,prix,site,date,heure))
  df <- rbind(df,res)
  Sys.sleep(2)
}


#PARAMARKET
#ESSAYER DE TROUVER AFF 48 /PAGES
site <- "paramarket"

for(i in 1:9) {
  url <- paste0("http://www.paramarket.com/",marque,"-m-27_page_",i,".html")
  page <- html(url)
  name <- page %>% html_nodes("div#bloc_galerie div.div_gal_para div a") %>% html_text()
  name <- name[name != ""]
  brand <- name[seq(2,length(name),by=3)]
  name <- name[seq(3,length(name),by=3)]
  name <- gsub("\t","",name)
  name <- gsub("\n","",name)
  brand <- gsub("\t","",brand)
  brand <- gsub("\n","",brand)
  name <- paste(brand, name)
  prix <- page %>% html_nodes("div#bloc_galerie div.div_gal_para div") %>% html_text()
  prix <- prix[(regexpr("[0-9]",prix) != -1)]
  prix <- prix[nchar(prix) < 22]
  prix <- gsub("\t","",prix)
  prix <- gsub("\n","",prix)
  res <- data.frame(cbind(name,prix,site,date,heure))
  df <- rbind(df,res)
  Sys.sleep(2)
}

#EPARAPHARMACIE

site <- "e parapharmacie"

for (i in 1:24) {
  url <- paste0("http://www.e-parapharmacie.fr/results.cfm?man_id=0&k=",marque,"&p=",i)
  page <- html(url)
  name <- page %>% html_nodes("td[valign='top'] a.txtGreen10") %>% html_text()
  name <- name[seq(1,16,by= 2)]
  prix <- page %>% html_nodes("span.txtGreenBold12,span.txtOrangeBold12") %>% html_text()
  prix <- prix[regexpr("[0-9]",prix) != -1]
  name <- gsub("Ãˆ","e",name)
  res <- data.frame(cbind(name,prix,site,date,heure))
  df <- rbind(df,res)
  Sys.sleep(2)
}


#MISE EN FORME




#PRIX
df$prix <- gsub("Â â‚¬","",df$prix,fixed = T)
df$prix <- gsub("â‚¬","",df$prix,fixed = T)
df$prix <- gsub("Â","",df$prix,fixed = T)
for (i in 1:20) df$prix <- gsub("  "," ",df$prix,fixed = T) 
df$prix <- gsub(" ","",df$prix,fixed = T)
df$prix <- gsub("\n","",df$prix,fixed = T)
df$prix <- gsub("\t","",df$prix,fixed = T)
df$prix <- gsub("\r","",df$prix,fixed = T)
df$prix <- as.numeric(gsub("\\,","\\.",df$prix))
df$prix <- gsub("\\.","\\,",df$prix)

#NOM PRODUIT
for (i in 1:20) df$name <- gsub("  "," ",df$name,fixed = T)
df$name <- gsub("ƒ","",df$name)
df$name <- gsub("\n","",df$name)
df$name <- gsub("\t","",df$name)
df$name <- gsub("\r","",df$name)
df$name <- gsub("è","E",df$name)
df$name <- gsub("Ã‰","E",df$name,fixed = T)
df$name <- gsub("Ã¨","E",df$name)
df$name <- gsub("Ãˆ","E",df$name,fixed = T)
df$name <- gsub("Ã¨","E",df$name,fixed = T)
df$name <- gsub("Ã¯","I",df$name,fixed = T)
df$name <- gsub("Ã©","E",df$name,fixed = T)
df$name <- gsub("Ã»","U",df$name,fixed = T)
df$name <- gsub("Ãª","E",df$name,fixed = T)
df$name <- gsub("Ã¨","e",df$name,fixed=T)
df$name <- gsub("Â©","e",df$name,fixed= T) 
df$name <- gsub("Â¨","e",df$name, fixed= T)  
df$name <- gsub("Â°"," ",df$name,fixed = T)
df$name <- gsub("Ã®","I",df$name,fixed = T)
df$name <- gsub("Ã©","E",df$name,fixed = T)
df$name <- gsub("Ã´","O",df$name,fixed = T)
df$name <- gsub("Ã‹","E",df$name,fixed = T)
df$name <- gsub("Â„¢"," ",df$name,fixed = T)
df$name <- gsub("Â€™"," ",df$name,fixed = T)
df$name <- gsub("Ã¢","A",df$name,fixed = T)
df$name <- gsub("Ã‚","A",df$name,fixed = T)
df$name <- gsub("Ã§","C",df$name,fixed = T)
df$name <- gsub("ÃŠ","E",df$name,fixed = T)
df$name <- gsub("Ã¤","A",df$name,fixed = T)
df$name <- gsub("Ãˆ","E",df$name,fixed = T)
df$name <- gsub("¶","A",df$name,fixed = T)
df$name <- gsub("Ã","A",df$name,fixed = T)

df$name <- gsub("XERA +","XERA+",df$name,fixed = T)
df$name <- gsub("STHEAL +","STHEAL+",df$name,fixed = T)
df$name <- gsub("SPF +","STHEAL+",df$name,fixed = T)

df$name <- toupper(df$name)
df$name <- gsub("È","E",df$name,fixed = T)
df$name <- gsub("É","E",df$name,fixed = T)
df$name <- gsub("À","A",df$name,fixed = T)
df$name <- gsub("Û","U",df$name,fixed = T)
df$name <- gsub("Ê","E",df$name,fixed = T)
df$name <- gsub("Â","A",df$name,fixed = T)


df$gamme <- "NR"
df$produit <- "NR"
df$contenance <- "NR"
df$galenique <- "NR"
df$unite <- "NR"
df$uti <- "NR"

#CONTENANCE et POIDS
cont <- c("500","1,7","1.7","400","300","200","150","125","100","75","50","40","30","25","20","15","10","7","1,2","1.2","1")
cont <- rev(cont)

for( c in cont) {
  df$contenance <- ifelse(regexpr(paste0(c," ML"),df$name,fixed=T) != -1,paste0(c," ML"),df$contenance)
  df$contenance <- ifelse(regexpr(paste0(c,"ML"),df$name,fixed=T) != -1,paste0(c," ML"),df$contenance)
}

po <- c("100","10","9,5","9.5","9.50","9,50","9","8","4","3.5","3,5","3","1.19","1,19","1.45","1,45","1","0,25","0.25")
po <- rev(po)

for( p in po) {
  df$contenance <- ifelse(regexpr(paste0(p," G"),df$name,fixed=T) != -1,paste0(p," G"),df$contenance)
  df$contenance <- ifelse(regexpr(paste0(p,"G"),df$name,fixed=T) != -1,paste0(p," G"),df$contenance)
}
df$contenance <- gsub("\\.","\\,",df$contenance)

df <- df[!is.na(df$name),]
df <- df[regexpr("AVENE",df$name) != -1,]

#UTILISATION
df$uti <- ifelse(regexpr("JOUR",df$name) != -1,"JOUR",df$uti)
df$uti <- ifelse(regexpr("NUIT",df$name) != -1,"NUIT",df$uti)

#GALENIC
df$galenique <- ifelse(regexpr("CREME",df$name) != -1,"CREME",df$galenique)
df$galenique <- ifelse(regexpr("MULSION",df$name) != -1,"EMULSION",df$galenique)
df$galenique <- ifelse(regexpr("LAIT",df$name) != -1,"LAIT",df$galenique)
df$galenique <- ifelse(regexpr("LOTION",df$name) != -1,"LOTION",df$galenique)
df$galenique <- ifelse(regexpr("BAUME",df$name) != -1,"BAUME",df$galenique)
df$galenique <- ifelse(regexpr("PAIN",df$name) != -1,"PAIN",df$galenique)
df$galenique <- ifelse(regexpr("CREME",df$name) != -1,"CREME",df$galenique)
df$galenique <- ifelse(regexpr("GEL",df$name) != -1,"GEL",df$galenique)
df$galenique <- ifelse(regexpr("SPRAY",df$name) != -1,"SPRAY",df$galenique)
df$galenique <- ifelse(regexpr("CRAYON",df$name) != -1,"CRAYON",df$galenique)
df$galenique <- ifelse(regexpr("CORRECT",df$name) != -1,"CORRECTEUR",df$galenique)
df$galenique <- ifelse(regexpr("STICK",df$name) != -1,"STICK",df$galenique)
df$galenique <- ifelse(regexpr(" EAU",df$name) != -1,"EAU",df$galenique)
df$galenique <- ifelse(regexpr("MOUSSE",df$name) != -1,"MOUSSE",df$galenique)
df$galenique <- ifelse(regexpr("MASQ",df$name) != -1,"MASQUE",df$galenique)
df$galenique <- ifelse(regexpr("MASK",df$name) != -1,"MASQUE",df$galenique)
df$galenique <- ifelse(regexpr("SOIN",df$name) != -1,"SOIN",df$galenique)
df$galenique <- ifelse(regexpr("MASCAR",df$name) != -1,"MASCARA",df$galenique)
df$galenique <- ifelse(regexpr("FOND DE T",df$name) != -1,"FOND DE TEINT",df$galenique)
df$galenique <- ifelse(regexpr("HUILE",df$name) != -1,"HUILE",df$galenique)
df$galenique <- ifelse(regexpr("SERUM",df$name) != -1,"SERUM",df$galenique)
df$galenique <- ifelse(regexpr("FLUIDE",df$name) != -1,"FLUIDE",df$galenique)
df$galenique <- ifelse(regexpr("POUDRE",df$name) != -1,"POUDRE",df$galenique)


#UNITE DE PRODUITS
df$unite <- ifelse(regexpr("X2",df$name) != -1,"+2",df$unite)
df$unite <- ifelse(regexpr("2 X",df$name) != -1,"+2",df$unite)
df$unite <- ifelse(regexpr("X 2",df$name) != -1,"+2",df$unite)
df$unite <- ifelse(regexpr("2 FLAC",df$name) != -1,"+++2",df$unite)
df$unite <- ifelse(regexpr("2 PAIN",df$name) != -1,"+++2",df$unite)
df$unite <- ifelse(regexpr("2X",df$name) != -1,"+2",df$unite)
df$unite <- ifelse(regexpr("DUO",df$name) != -1,"+2",df$unite)
df$unite <- ifelse(regexpr("X3",df$name) != -1,"+3",df$unite)
df$unite <- ifelse(regexpr("3X",df$name) != -1,"+3",df$unite)
df$unite <- ifelse(regexpr("TRIO",df$name) != -1,"+3",df$unite)
df$unite <- ifelse(regexpr(" + ",df$name,fixed=T) != -1,"+2",df$unite)
df$unite <- ifelse(regexpr(" LOT ",df$name,fixed=T) != -1,"LOT",df$unite)
df$unite <- ifelse(regexpr("PACK",df$name,fixed=T) != -1,"PACK",df$unite)
df$unite <- ifelse(regexpr("COFFRET",df$name,fixed=T) != -1,"COFFRET",df$unite)
df$unite <- ifelse(regexpr("BOX",df$name,fixed=T) != -1,"BOX",df$unite)
df$unite <- ifelse(regexpr("TROUSSE",df$name,fixed=T) != -1,"TROUSSE",df$unite)
df$unite <- ifelse(regexpr("CORBEILLE",df$name,fixed=T) != -1,"CORBEILLE",df$unite)



#SEGMENT DU PRODUIT

df$produit <- ifelse(regexpr("SPF",df$name) != -1,"SPF",df$produit)
df$produit <- ifelse(regexpr("GEL DOUCHE DO",df$name) != -1,"GEL DOUCHE DOUCEUR",df$produit)
df$produit <- ifelse(regexpr("COLD CREAM",df$name) != -1,"COLD CREAM",df$produit)
df$produit <- ifelse(regexpr("HOMME",df$name) != -1,"HOMME",df$produit)
df$produit <- ifelse(regexpr("LOTION MICE",df$name) != -1,"LOTION MICELLAIRE",df$produit)
df$produit <- ifelse(regexpr("COMPACT",df$name) != -1,"COMPACT",df$produit)
df$produit <- ifelse(regexpr("CREME MINE",df$name) != -1,"CREME MINERALE",df$produit)
df$produit <- ifelse(regexpr("LLANT DOUCEUR",df$name) != -1,"DEMAQUILLANT DOUCEUR",df$produit)
df$produit <- ifelse(regexpr("GOMMAGE",df$name) != -1,"GOMMAGE",df$produit)
df$produit <- ifelse(regexpr("EAU MICELL",df$name) != -1,"CLEANANCE EAU MICELLAIRE",df$produit)
df$produit <- ifelse(regexpr("APAIS",df$name) != -1,"APAISANT",df$produit)
df$produit <- ifelse(regexpr("DEODO",df$name) != -1,"DEODORANT",df$produit)
df$produit <- ifelse(regexpr("NUTRI",df$name) != -1,"NUTRITIVE",df$produit)
df$produit <- ifelse(regexpr("DOUCEUR",df$name) != -1,"DOUCEUR",df$produit)
df$produit <- ifelse(regexpr("HAUTE PRO",df$name) != -1,"HAUTE PROTECTION",df$produit)
df$produit <- ifelse(regexpr("MATIFIANT",df$name) != -1,"MATIFIANT",df$produit)
df$produit <- ifelse(regexpr("LAIT MINE",df$name) != -1,"LAIT MINERAL",df$produit)
df$produit <- ifelse(regexpr("HUILE CORP",df$name) != -1,"HUILE CORPORELLE",df$produit)
df$produit <- ifelse(regexpr("MOUSSE NET",df$name) != -1,"MOUSSE NETTOYANTE",df$produit)
df$produit <- ifelse(regexpr(" MEN ",df$name) != -1,"AVENE MEN",df$produit)
df$produit <- ifelse(regexpr("APAISANT",df$name) != -1,"APAISANT",df$produit)
df$produit <- ifelse(regexpr("GOMMAGE CO",df$name) != -1,"AVENE GOMMAGE CORPS",df$produit)
df$produit <- ifelse(regexpr("RASER",df$name) != -1,"RASAGE",df$produit)
df$produit <- ifelse(regexpr("RASAG",df$name) != -1,"RASAGE",df$produit)
df$produit <- ifelse(regexpr("DERMO-K",df$name) != -1,"DERMO-K",df$produit)
df$produit <- ifelse(regexpr("REPARATEUR",df$name) != -1,"REPARATEUR",df$produit)
df$produit <- ifelse(regexpr("AUTOBRON",df$name) != -1,"AUTOBRONZANT",df$produit)
df$produit <- ifelse(regexpr("CRAYON",df$name) != -1,"CRAYON",df$produit)
df$produit <- ifelse(regexpr("LEVRE",df$name) != -1,"LEVRES",df$produit)
df$produit <- ifelse(regexpr("PINCEAU",df$name) != -1,"PINCEAU",df$produit)
df$produit <- ifelse(regexpr("TOLERANCE EXT",df$name) != -1,"TOLERANCE EXTREME",df$produit)
df$produit <- ifelse(regexpr("CICALFATE",df$name) != -1,"CICALFATE",df$produit)
df$produit <- ifelse(regexpr("EAU T",df$name) != -1,"EAU THERMALE",df$produit)
df$produit <- ifelse(regexpr("TRIX",df$name) != -1,"TRIXERA",df$produit)
df$produit <- ifelse(regexpr("CLEANANCE",df$name) != -1,"CLEANANCE",df$produit)
df$produit <- ifelse(regexpr("XERAC",df$name) != -1,"XERACALM",df$produit)
df$produit <- ifelse(regexpr("TRIACNEA",df$name) != -1,"TRIACNEAL",df$produit)
df$produit <- ifelse(regexpr("COUVR",df$name) != -1,"COUVRANCE",df$produit)
df$produit <- ifelse(regexpr("PHYSIOLI",df$name) != -1,"PHYSIOLIFT",df$produit)
df$produit <- ifelse(regexpr("PEDIAT",df$name) != -1,"PEDIATRIL",df$produit)
df$produit <- ifelse(regexpr("PEDITA",df$name) != -1,"PEDIATRIL",df$produit)
df$produit <- ifelse(regexpr("DIATRIL",df$name) != -1,"PEDIATRIL",df$produit)
df$produit <- ifelse(regexpr("ANTIR",df$name) != -1,"ANTIROUGEUR",df$produit)
df$produit <- ifelse(regexpr("ANTI RO",df$name) != -1,"ANTIROUGEUR",df$produit)
df$produit <- ifelse(regexpr("ANTI-RO",df$name) != -1,"ANTIROUGEUR",df$produit)
df$produit <- ifelse(regexpr("PEAUX IN",df$name) != -1,"PEAUX INTOLERANTES",df$produit)
df$produit <- ifelse(regexpr("PEAU IN",df$name) != -1,"PEAUX INTOLERANTES",df$produit)
df$produit <- ifelse(regexpr("SOLAIRE",df$name) != -1,"SOLAIRE",df$produit)
df$produit <- ifelse(regexpr("AKER",df$name) != -1,"AKERAT",df$produit)
df$produit <- ifelse(regexpr("HYDRANCE",df$name) != -1,"HYDRANCE",df$produit)
df$produit <- ifelse(regexpr("DENSEAL",df$name) != -1,"DENSEAL",df$produit)
df$produit <- ifelse(regexpr("YST",df$name) != -1,"YSTHEAL",df$produit)
df$produit <- ifelse(regexpr("ELUAGE",df$name) != -1,"ELUAGE",df$produit)
df$produit <- ifelse(regexpr("D-PIGM",df$name) != -1,"D-PIGMENT",df$produit)
df$produit <- ifelse(regexpr("D PIGM",df$name) != -1,"D-PIGMENT",df$produit)
df$produit <- ifelse(regexpr("SERENAGE",df$name) != -1,"SERENAGE",df$produit)

rm(list = ls()[ls() != "df"])

write.csv2(df,"C:/Users/ottavig/Desktop/extract_prix_avene.csv",row.names = F)