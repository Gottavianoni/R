#FUT
library(RSelenium)
library(rvest)
#SANS PROXY POUR UTILISER LE PHANTOMJS
Sys.setenv(https_proxy = "")
Sys.setenv(http_proxy = "")

tournois <- c("LES BANDITS"   
  ,"Les PiedsCarr?s"
  ,"bamb78"
  ,"SUPRA AUTEUIL"
  ,"Campsas"
  ,"FC DRIBA"
  ,"French Tooch"
  ,"LES SANS PAPIER"
  ,"Little B FC"
  ,"Pepe SG")

for(club in tournois) {
club <- gsub("\\s","%20",club)

#ESPACE %20
phantom(pjs_cmd = "C:/Users/ottavig/Documents/phantomjs.exe",extra = "--proxy-auth=***** --proxy=*****")
remDr <- remoteDriver(browserName = "phantomjs")
remDr$open()
url <- paste0("https://www.easports.com/iframe/fifa16proclubs/PS4/",club)
remDr$navigate(url)
Sys.sleep(2)
page <- read_html(remDr$getPageSource()[[1]])
match <- sum(as.numeric(page %>% html_nodes("div div[ng-show='clubStats'] span[class='stat-num ng-binding']") %>% html_text()))
url <- paste0("https://www.easports.com/iframe/fifa16proclubs/PS4/",club,"/members")
remDr$navigate(url)
Sys.sleep(2)
page <- read_html(remDr$getPageSource()[[1]])
stats <- page %>% html_nodes("ul div[class='member-list-data stat-box'] div span") %>% html_text()
names <- page %>% html_nodes("ul.member-list div div.media-body h1") %>% html_text()

gp <- stats[which(stats == "gp") -1]
gls <- stats[which(stats == "gls") -1]
ast <- stats[which(stats == "ast") -1]
pass <- stats[which(stats == "pass") -1]
pra <- stats[which(stats == "p%") -1]
tkl <- stats[which(stats == "tkl") -1]
tklra <- stats[which(stats == "tkl%") -1]
wra <- stats[which(stats == "w%") -1]

res <- data.frame(cbind(names,gls,gp,ast,pass,pra,tkl,tklra,wra))
for(c in names(res)[-1]) res[,c] <- as.numeric(substr(res[,c],regexpr("[0-9]{1,}",res[,c]),regexpr("[0-9]{1,}",res[,c]) + attr(regexpr("[0-9]{1,}",res[,c]),"match.length")-1))

res$glspm <- round(res$gls  * 100/ res$gp)
res$astpm <- round(res$ast * 100/ res$gp)
res$tklpm <- round(res$tkl / res$gp,1)
res$gppm <-  round(res$gp *100/ match)
res$passpm <-  round(res$pass / res$gp)

assign(paste0("stats_",gsub("%20","",club)),res)
remDr$close()
closeAllConnections()
}

rm(list = ls()[substr(ls(),1,6) != "stats_"])
equi <- ls()
nom_equipe <- substr(equi,7,nchar(equi))

for (team in equi) {
  df <- get(team)
  df[,15] <- nom_equipe[which(team == equi)]
  assign(team,df)
}

rm(list = ls()[substr(ls(),1,6) != "stats_"])
team <- ls()
table <- NULL
for(equi in team) {
table <- rbind(table,get(equi))
}


for (c in 2:14) {
df <- data[order(as.numeric(data[,c]),decreasing = T),c(1,c,15)]
df <- df[1:20,]
assign(paste0("cl_",names(table)[c]),df)
} 
data <- table[-1,]
  

table <- rbind(c("Joueur","Buts","Match","PasseD","% Passes reussies","Tacles","% de Tacles reussi","% de Victoire","% de Chance Marquer dans un Match","% de Chance de faire une passe D dans un Match","% de Match joue","Passes par Match","equipe"),table)
