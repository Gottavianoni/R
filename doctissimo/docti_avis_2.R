#SCRAPING DOCTISSIMO RECUP DES ARTICLES
library(rvest)

#FORUM
liste_forum<- c("sante","maquillage","grossesse-bebe","mode","forme-beaute","nutrition","psychologie","doctissimo","loisirs","people-stars","medicaments","forme-sport","viepratique","animaux","famille","cuisine","environnement")
produit <- "avene"
liste_forum <- paste("http://forum.doctissimo.fr/search_result.php?orderSearch=topic_date&groupbySearch=1&config=",liste_forum,".inc&author=&authorlocation=1&search=",produit,"&resSearch=1000&daterange=1&subcat=0&searchtype=1&trash=0&trash_post=0&moderation=0",sep ="")
#----------------------------------------
#Terme de la recherche
#Pour selectionner seulement les article avec titres du nom titre=1 ou titre=4 pour les messages

for (research in liste_forum) {
  
html_research <- read_html(research)
config <- substr(research,regexpr("earch=1",research,fixed = T) + 15,regexpr(".inc",research,fixed = T)-1)
sous_cat_article <- html_research %>% html_nodes("form[method='post'] select[name='cat'] optgroup option") %>% html_attr("value")
sous_cat_article <- gsub("*forme-beaute.inc","",sous_cat_article,fixed = T)
sous_cat_article <- paste("http://forum.doctissimo.fr/search_result.php?cat=",sous_cat_article,"&orderSearch=topic_date&groupbySearch=1&config=",config,".inc&author=&authorlocation=1&search=",produit,"&resSearch=1000&daterange=1&subcat=0&searchtype=4&trash=0&trash_post=0&moderation=0",sep ="")

for (sub in sous_cat_article) {
html_research <- try(html(sub))
liens_article <- html_research %>% html_nodes("table#block_topics_list td.sujetCase3 a[rel='nofollow']") %>% html_attr("href")
if(!exists("liste_article")) liste_article <- liens_article else liste_article <- c(liste_article,liens_article)
Sys.sleep(3)
print(paste0("Il reste ",length(sous_cat_article) - which(sub == sous_cat_article)," sur ",length(sous_cat_article)))
save.image("C:/Users/ottavig/Documents/docti.RData")
}
if(!exists("all_liste_article")) all_liste_article <- liste_article else all_liste_article <- c(all_liste_article,liste_article)
rm(liste_article)
save.image("C:/Users/ottavig/Documents/docti.RData")
print(paste0("Il reste ",length(liste_forum) - which(research == liste_forum)," sur ",length(liste_forum)))
}

all_liste_article <- unique(all_liste_article)
all_liste_article <- substr(all_liste_article,1,regexpr(".htm",all_liste_article, fixed = T)+3)

rm(config,liste_forum,produit,research,sous_cat_article,sub,html_research,liens_article)

#-------------------------------
#NOT CHECK bouclez pour r?cup?rer tous les avis !!!!!
#-------------------------------

#Premiere Boucle

for (col in names(content_art)) {
content_art[,col] <- gsub("?","e",content_art[,col])
content_art[,col] <- gsub("?","e",content_art[,col])
content_art[,col] <- gsub("?","e",content_art[,col])
content_art[,col] <- gsub("?","a",content_art[,col])
content_art[,col] <- gsub("?","c",content_art[,col])
content_art[,col] <- gsub("?","a",content_art[,col])
content_art[,col] <- gsub("?","u",content_art[,col])
content_art[,col] <- tolower(content_art[,col])
}

rm(agent,url_page,url_page_vide,nom_article,i,content_page,liens_article,html_page,article,nb_pages)