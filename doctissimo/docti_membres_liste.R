#Utilisateurs Doctissimo
library(rvest)
load("C:/Users/ottavig/Documents/docti_membre.RData")

Sys.setenv(http_proxy="*****")
Sys.setenv(https_proxy="*****")
Sys.setenv(encoding = "UTF-8")

if (!exists("uti")){
uti <- liste_membre
uti <- uti[which(unique(as.character(uti)) != "Profil supprimé")]
uti <- paste("http://club.doctissimo.fr/",uti,"/",sep="")
uti <- gsub("é","e",uti, fixed = T)
uti <- gsub("è","e",uti, fixed = T)
uti <- gsub("ê","e",uti, fixed = T)
uti <- gsub("ë","e",uti, fixed = T)
uti <- gsub(" ","-",uti,fixed = T)
uti <- gsub("./","",uti,fixed = T)
}

for (pers in uti) {
if(typeof(try(html(pers),silent= T)) == "externalptr") {
html_uti <- html(pers)
nom_uti <- html_uti %>% html_nodes("h1#user_pseudo") %>% html_text()
grade_uti <- html_uti %>% html_nodes("div.user_infos span.user_grade") %>% html_text()
grade_uti <- if(length(grade_uti) == 0) "NonRens" else grade_uti
sexe_uti <- html_uti %>% html_nodes("div.small_column_left_panel img") %>% html_attr("alt")
sexe_uti <- if(length(sexe_uti) == 0) "NonRens" else sexe_uti
date_uti <- try(html_uti %>% html_nodes("div.small_column_left div.small_column_left_panel") %>% .[[2]] %>% html_text(),silent = T)
date_uti <- substr(date_uti,regexpr("[0-9][0-9][0-9][0-9]",date_uti,perl= T),regexpr("[0-9][0-9][0-9][0-9]",date_uti,perl= T) + 3)
date_uti <- if(date_uti == "") "NonRens" else date_uti
date_uti <- if(substr(date_uti,1,2) == "fa") "NonRens" else date_uti
zone_uti <- html_uti %>% html_nodes("div.small_column_left_shadow div#more_informations") %>% html_text()
zone_uti <- gsub("(\\r)","",zone_uti,perl = T)
zone_uti <- gsub("(\\t)","",zone_uti,perl = T)
zone_uti <- gsub("(\\n)","",zone_uti,perl = T)
zone_uti <- substr(zone_uti,18,nchar(zone_uti))
zone_uti <- if(length(zone_uti) == 0) "NonRens" else zone_uti
visit_uti <- html_uti %>% html_nodes("div.small_column_left_shadow ul li strong") %>% html_text()
msg_uti <- visit_uti[2]
visit_uti <- visit_uti[1]

profil_uti <- data.frame(cbind(nom_uti,sexe_uti,date_uti,grade_uti,zone_uti,visit_uti,msg_uti))

if (!exists("docti_uti")) docti_uti <- profil_uti else if(typeof(try(rbind(docti_uti,profil_uti),silent = T)) == "list")docti_uti <- rbind(docti_uti,profil_uti)
rm(profil_uti)
}
print(paste0("Il reste ", length(uti) - which(pers == uti) , " sur ", length(uti)))
save.image("C:/Users/ottavig/Documents/docti_membre.RData")
Sys.sleep(3)
}

rm(nom_uti,date_uti,grade_uti,sexe_uti,msg_uti,visit_uti,zone_uti,profil_uti,pers,html_uti)

#------
docti_uti <- unique(docti_uti)
for (i in 2:length(docti_uti$nom_uti)) if (docti_uti$nom_uti[i] == docti_uti$nom_uti[(i-1)]) docti_uti <- docti_uti[-i + 1 ,]
docti_uti$date_uti <- ifelse(nchar(as.character(docti_uti$date_uti)) != 4, "NonRens", as.character(docti_uti$date_uti))
docti_uti$zone_uti <- ifelse(nchar(as.character(docti_uti$zone_uti)) > 40 ,"NonRens", as.character(docti_uti$zone_uti))
docti_uti$zone_uti <- ifelse(regexpr("[0-9][0-9][0-9][0-9][0-9]",docti_uti$zone_uti,perl= T) != -1, substr(docti_uti$zone_uti,regexpr("[0-9][0-9]",docti_uti$zone_uti,perl= T),regexpr("[0-9][0-9]",docti_uti$zone_uti,perl= T) + 1),docti_uti$zone_uti)
docti$date_msg <- ifelse(regexpr("[0-9][0-9][0-9][0-9]",docti$date_msg,perl= T) != -1, substr(docti$date_msg,regexpr("[0-9][0-9][0-9][0-9]",docti$date_msg,perl= T),regexpr("[0-9][0-9][0-9][0-9]",docti$date_msg,perl= T) + 3),docti$date_msg)
#A TERMINER
for (c in names(docti_uti)) docti_uti[,c] <- as.character(docti_uti[,c])
res <- docti_uti[1,]
j <- 2
var <- docti_uti[1,"nom_uti"]


for (i in 2:length(docti_uti$nom_uti)) {
  
  if (var != docti_uti[i,"nom_uti"]) {
    res[j,] <- docti_uti[i,]
    j <- j+1
  }
  var <- docti_uti[i,"nom_uti"]

}



require(httr)
require(RCurl)
require(RJSONIO)

full_url <- oauth_callback()
full_url <- gsub("(.*localhost:[0-9]{1,5}/).*", x=full_url, replacement="\\1")
print(full_url)
app_name <- "instaR"
client_id <- "73ffb23e19ea4bcea82d48284b665ca1"
client_secret <- "cf785d5ca9084772bf47c5c10375a965"
scope <- "basic"
instagram <- oauth_endpoint(
  authorize = "https://api.instagram.com/oauth/authorize",
  access = "https://api.instagram.com/oauth/access_token")
myapp <- oauth_app(app_name, client_id, client_secret)
ig_oauth <- oauth2.0_token(instagram, myapp,scope="basic",  type = "application/x-www-form-urlencoded",cache=FALSE)
tmp <- strsplit(toString(names(ig_oauth$credentials)), '"')
token <- tmp[[1]][4]

user_info <- fromJSON(getURL(paste('https://api.instagram.com/v1/users/search?q=',username,'&access_token=',token,sep="")),unexpected.escape = "keep")
user_followed <- fromJSON(getURL(paste('https://api.instagram.com/v1/users/37721750/followed_by?access_token=',token,sep="")),unexpected.escape = "keep")

username <- "ninonchka"
received_profile <- user_info$data[[1]]
library(instaR)

load("my_oauth")
mccain <- getFollowers(username, token)
test <- getUser(username = "ninonchka",token)
obama <- getUser( userid = 269505098, token = token)
obama <- searchInstagram( tag="", token=my_oauth, n=100, folder="obama")
?instaR
obama <- getFollowers( username="bysuzette", token=token)


instaOAuth

token <- oauth2.0_token(instagram, myapp, cache = FALSE, 
                        scope = scope)
if (GET(paste0("https://api.instagram.com/v1/users/self/feed?count=1", 
               "&access_token=", token$credentials$access_token))$status == 
    200) message("Authentication successful.")
