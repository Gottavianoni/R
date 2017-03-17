#SCRAPING DOCTISSIMO RECUP DES ARTICLES
library(httr)
library(RCurl)
library(rvest)

prox <- use_proxy(url = "proxy1",port = 8080,username = "ottavig", password = "Pierre-Fabre01")
agent <- user_agent("Mozilla/5.0 (compatible; Googlebot/2.1; +http://www.google.com/bot.html)")

#----------------------------------------
#Terme de la recherche
produit <- "neogenic"
#Catalogue 52 = Calvitie et perte de cheveux
catalog <- "52"
#Pour selectionner seulement les article avec titres du nom titre=1 ou titre=4 pour les messages
#----------------------------------------

research = "http://forum.doctissimo.fr/search_result.php?cat=xx&orderSearch=topic_date&groupbySearch=1&config=sante.inc&author=&authorlocation=1&search=xxxxx&titre=1&resSearch=5000&daterange=1&subcat=0&searchtype=1&trash=0&trash_post=0&moderation=0"
research <- gsub("?cat=xx",paste("?cat=",catalog,sep=""),research,fixed = T)
research <- gsub("&search=xxxxx",paste("&search=",produit,sep= ""),research,fixed = T)
html_research <- html(research,prox,agent)
liens_article <- html_research %>% html_nodes("table#block_topics_list td.sujetCase3 a.cCatTopic") %>% html_attr("href")
rm(research,catalog,produit,html_research)

#Premiere Boucle
for (article in liens_article) {
#recup du nombre de pages
html_article <- html(article,prox,agent)
nom_article <- html_article %>% html_nodes("div#topic table.main th.padding div.left h3") %>% html_text()
nb_pages <- html_article %>% html_nodes("div#topic table.main div.pagination_main_visible a.cHeader") %>% html_text()
if(nb_pages == "-Inf" || length(nb_pages)==0 || is.na(nb_pages) ) nb_pages <- 1 else nb_pages <- max(as.numeric(nb_pages[which(nb_pages != "...")]))

rm(html_article)

url_page_vide <- strsplit(article,"_")[[1]]
url_page_vide <- substr(article,1,nchar(article)-nchar(url_page_vide[length(url_page_vide)]))

if (!exists("index")) index <- 1
#Boucle pour tous les articles
for (i in index:nb_pages){

url_page <- paste(url_page_vide,i,".htm",sep="")
html_page <- html(url_page,prox,agent)
html_page <- html_page %>% html_nodes("div#topic table.messagetable")
auteur_msg <-  html_page %>% html_nodes("td.messCase1 b.s2") %>% html_text()
if (length(date_msg <- html_page %>% html_nodes("td.messCase2 span.topic_posted") %>% html_text()) == 0){
date_msg <- html_page %>% html_nodes("td.messCase2 div.toolbar div.left") %>% html_text()
}
avis_msg <- html_page %>% html_nodes("td.messCase2 div.toolbar + div") %>% html_text()
avis_msg <- avis_msg[which(regexpr("OAS_AD",avis_msg,fixed = F) == "-1")]
like_msg <- html_page %>% html_nodes("div.rating_block span.counter") %>% html_text()

content_page <- data.frame(cbind(auteur_msg,date_msg,nom_article,like_msg,avis_msg)) 
rm(auteur_msg,date_msg,avis_msg,like_msg) 

if (!exists("content_art")) content_art <- content_page else content_art <- rbind(content_art,content_page) 
if(memory.size() >= 200) {
index <- i
liens_article <- liens_articles[which(liens_article == article),length(liens_article)]
break()
Sys.sleep(4)
}
}
if (!exists("content_research")) content_research <- content_art else content_research <- rbind(content_research,content_art) 
rm(content_art,index)
}

rm(agent,prox,url_page,url_page_vide,nom_article,i,content_page,liens_article,html_page,article,nb_pages)
