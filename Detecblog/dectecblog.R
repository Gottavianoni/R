#100 premiere page google
library(rvest)

#TERMES DE LA RECHARCHES GOOGLE
ville = "toulouse"
theme = "cosmetique"

q <- paste0("https://www.google.fr/search?num=100&q=blog+",theme,"+",ville)

#EXTRACTION DU CONTENU GOOGLE
research <- html(q)
liens <- research %>% html_nodes("div#search a") %>% html_attr("href")
liens <- substr(liens,regexpr("\\:http\\:\\/\\/+|\\:https\\:\\/\\/+",liens),nchar(liens))
liens <- gsub("%2525C3%2525A8","e",liens)
liens <- gsub("%2525C3%2525A9","e",liens)
liens <- gsub("%25C3%25A9","e",liens)
liens <- gsub("%25C3%25A9","e",liens)
liens <- gsub("\\,","-",liens)
liens <- substr(liens,2,regexpr("%",liens)-1)
liens <- liens[liens != ""]
liens <- liens[regexpr("url",liens) == -1]
liens <- liens[regexpr("search?",liens,fixed=T) == -1]

ind <- gregexpr("/",liens)
lien_f <- NULL
for (i in 1:length(ind)) {
  if(length(ind[[i]]) == 3)  lien_f <- c(lien_f,liens[i])
}
liens <- lien_f

rm(q,research,theme,ville,lien_f,ind,i)

for(blog in liens[1:10]) {
#PAGE DE BLOG et EXTRACTION DES RESEAUX SOCIAUX POUR FOLLOWERS
page <- html(blog)
links <- page %>% html_nodes("a") %>% html_attr("href")
twi <- links[which(regexpr("twitter",links,fixed = T) != -1)]
ins <- links[which(regexpr("instagram",links,fixed = T) != -1)]
fb <- links[which(regexpr("facebook",links,fixed = T) != -1)]

twi <- twi[regexpr("twitter.com",twi,fixed = T) != -1]
fb <- fb[regexpr("facebook.com",fb,fixed = T) != -1]
ins <- ins[regexpr("instagram.com",ins,fixed = T) != -1]

twi <- twi[nchar(twi) > 20 & nchar(twi) < 80]
fb <- fb[nchar(fb) > 20 & nchar(fb) < 80]
ins <- ins[nchar(ins) > 21 & nchar(ins) < 80]

fb <- unique(fb)
ins <- unique(ins)
twi <- unique(twi)
twi <- twi[regexpr("share",twi) == -1]

fl_twi <- tolower(substr(twi,regexpr("com",twi,fixed=T) + 4, nchar(twi)))
fl_ins <- tolower(substr(ins,regexpr("com",ins,fixed=T) + 4, nchar(ins)))
fl_fb <- tolower(substr(fb,regexpr("com",fb,fixed=T) + 4, nchar(fb)))

fl_twi <- substr(fl_twi,regexpr("[a-z]",fl_twi),nchar(fl_twi))

if(length(fb) == 1 & length(ins) == 1 & length(twi) == 1 ) {
  
ins_f <- ins
twi_f <- twi
fb_f <- fb

} else {

#rechercher du nom du blogger sur reseaux sociaux
if(length(fl_fb) == 1) {tst_soc <- fl_fb;fl_soc <- "f"}
if(length(fl_twi) == 1) {tst_soc <- fl_twi;fl_soc <- "t"}
if(length(fl_ins) == 1) {tst_soc <- fl_ins;fl_soc <- "i"}

tst_soc <- gsub("\\-","",tst_soc)
tst_soc <- gsub("\\_","",tst_soc)
tst_soc <- gsub("\\/","",tst_soc)
tst_soc <- gsub("\\#","",tst_soc)
tst_soc <- gsub("\\*","",tst_soc)
tst_soc <- gsub("\\.","",tst_soc)
tst_soc <- gsub("\\:","",tst_soc)

if(!is.na(match(tst_soc,fl_twi))) twi_f <- twi[match(tst_soc,fl_twi)[1]]
if(!is.na(match(tst_soc,fl_ins))) ins_f <- ins[match(tst_soc,fl_ins)[1]]
if(!is.na(match(tst_soc,fl_fb))) fb_f <- fb[match(tst_soc,fl_fb)[1]]

if(fl_soc == "t") twi_f <- twi
if(fl_soc == "i") ins_f <- ins
if(fl_soc == "f") fb_f <- fb

if(!exists("twi_f")) twi_f <- twi[agrep(tst_soc,fl_twi)[1]]
if(!exists("ins_f")) ins_f <- ins[agrep(tst_soc,fl_ins)[1]]
if(!exists("fb_f"))  fb_f <- fb[agrep(tst_soc,fl_fb)[1]]

if(is.na(twi_f) > 0) twi_f <- twi[1]
if(is.na(ins_f) > 0) ins_f <- ins[1]
if(is.na(fb_f) > 0) fb_f <- fb[1]
}

rm(fb,ins,twi,tst_soc,fl_soc)

#Evaluation de la qualite du blog via un score via mots impact
content <- page %>% html_text()
content <- as.character(content)
try(content <- tolower(content))
content <- gsub("é","e",content,fixed=T)
content <- gsub("è","e",content,fixed=T)
content <- gsub("ê","e",content,fixed=T)
content <- gsub("à","a",content,fixed=T)
content <- gsub("û","u",content,fixed=T)
content <- gsub("ç","c",content,fixed=T)

ind <- length(gregexpr("cosmeti",content,fixed=T)[[1]])
ind <- ind + length(gregexpr("creme",content,fixed=T)[[1]])
ind <- ind + length(gregexpr("soin",content,fixed=T)[[1]])
ind <- ind + length(gregexpr("peau",content,fixed=T)[[1]])
ind <- ind + length(gregexpr("visage",content,fixed=T)[[1]])
ind <- ind + length(gregexpr("texture",content,fixed=T)[[1]])
ind <- ind + length(gregexpr("odeur",content,fixed=T)[[1]])
ind <- ind + length(gregexpr("creme",content,fixed=T)[[1]])
ind <- ind + length(gregexpr("masque",content,fixed=T)[[1]])
ind <- ind + length(gregexpr("apaisant",content,fixed=T)[[1]])
ind <- ind + length(gregexpr("revitalisant",content,fixed=T)[[1]])
ind <- ind + length(gregexpr("beaute",content,fixed=T)[[1]])
ind <- ind + length(gregexpr("maquillage",content,fixed=T)[[1]])
ind <- ind + length(gregexpr("cheveux",content,fixed=T)[[1]])
ind <- ind + length(gregexpr("bouton",content,fixed=T)[[1]])
ind <- ind + length(gregexpr("imperfectio",content,fixed=T)[[1]])
ind <- ind + length(gregexpr("produit",content,fixed=T)[[1]])

ind <- ind - 17
ind <- ind*100/80
score_pertinence <- ind

#Evaluation de la qualite
ind <- length(gregexpr("grave",content,fixed=T)[[1]])
ind <- ind + length(gregexpr("trop",content,fixed=T)[[1]])
ind <- ind + length(gregexpr("fun",content,fixed=T)[[1]])
ind <- ind + length(gregexpr("cool",content,fixed=T)[[1]])
ind <- ind + length(gregexpr("nul",content,fixed=T)[[1]])
ind <- ind + length(gregexpr("pourri",content,fixed=T)[[1]])
ind <- ind + length(gregexpr("kiff",content,fixed=T)[[1]])
ind <- ind + length(gregexpr("tueri",content,fixed=T)[[1]])
ind <- ind + length(gregexpr("balle",content,fixed=T)[[1]])
ind <- ind + length(gregexpr("bombe",content,fixed=T)[[1]])
ind <- ind + length(gregexpr("boss",content,fixed=T)[[1]])
ind <- ind + length(gregexpr("chelou",content,fixed=T)[[1]])
ind <- ind + length(gregexpr("sape",content,fixed=T)[[1]])
ind <- ind + length(gregexpr("zap",content,fixed=T)[[1]])
ind <- ind + length(gregexpr("tune",content,fixed=T)[[1]])
ind <- ind + length(gregexpr("chelou",content,fixed=T)[[1]])
ind <- ind + length(gregexpr("mate",content,fixed=T)[[1]])

ind <- ind - 17
ind <- ind*100/80
score_adolescence <- ind

#Age Blogger
age <- gregexpr("[0-9][0-9] an",content)
age <- substr(content,age[[1]][1] - 10,age[[1]][1] + 10)
age <- substr(age,regexpr("[0-9][0-9]",age),regexpr("[0-9][0-9]",age)+1)

indice_age <- age

#DETECTION DES FOLLOWERS ET TYPE DE FOLLOWERS
#TWITTER
if(exists("fl_twi") & length(fl_twi)==1 ) {
  if(!is.na(fl_twi)) {
library(twitteR)
#to get your consumerKey and consumerSecret see the twitteR documentation for instructions
consumer_key <- "*****"
consumer_secret <- "*****"
access_token <- "*****"
access_secret <- "*****"
try(setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_secret),silent = T)
user <- getUser(fl_twi)
twi_loc <- user$location
twi_folcount <- user$followersCount
  } 
  }
  else {
  twi_crea <- "NC"
  twi_folcount <- "NC"
}

#INSTAGRAM
if(exists("ins_f") & length(ins_f)==1) {
  if (!is.na(ins_f)) {
insta <- read_html(ins_f)
fols <- insta %>% html_text()
ind <- regexpr("ollowed_by",fols)
fols <- substr(fols,ind,ind +30)
ind <- regexpr(":[0-9]*}",fols)
fols <- as.numeric(substr(fols,ind + 1,ind + attr(ind,"match.length") - 2))
}} else fols <- "NC"

#FACEBOOK
if(exists("fb_f") & length(fb_f) == 1 ) {
  if(!is.na(fb_f)) {
faceb <- read_html(fb_f)
like <- faceb %>% html_nodes("div a span span")
like <- faceb %>% html_text()
like <- substr(like,regexpr("[0-9]* personnes aim",like),regexpr("personnes aim",like)-2)
}} else like <- "NC"

if(!exists("blog"))
if(!exists("indice_age")) indice_age <- "NC"
if(!exists("twi_f")) twi_f <- "NC"
if(!exists("twi_crea")) twi_crea <- "NC"
if(!exists("twi_folcount")) twi_folcount <- "NC"
if(!exists("ins_f")) ins_f <- "NC"
if(!exists("fb_f")) fb_f <- "NC"
if(!exists("fols")) fols <- "NC"
if(!exists("like")) like <- "NC"
if(is.na(ins_f)) ins_f <- "NC"
if(is.na(fb_f)) fb_f <- "NC"
if(is.na(twi_f)) twi_f <- "NC"
    
resu <- c(blog,indice_age,score_pertinence,score_adolescence,twi_f,twi_loc,twi_folcount,ins_f,fols,fb_f,like)
if(!exists("eval_blog")) eval_blog <- t(data.frame(resu)) else eval_blog <- rbind(eval_blog,resu)
print(resu)
rm(blog,indice_age,score_adolescence,score_pertinence,twi_f,twi_crea,twi_folcount,ins_f,fols,fb_f,like,ins_f,page,resu,age,content,fb,fl_fb,fl_ins,fl_twi,twi,links,ind,ins,faceb,insta,user)
}
eval_blog <- data.frame(eval_blog)
names(eval_blog) <- c("blog","indice_age","score_pertinence","score_adolescence","twi_f","twi_loc","twi_folcount","ins_f","fols","fb_f","like")
for (c in names(eval_blog)) eval_blog[,c] <- as.character(eval_blog[,c])
write.csv2(eval_blog,"Y:/eval_blog.csv",row.names = F)

