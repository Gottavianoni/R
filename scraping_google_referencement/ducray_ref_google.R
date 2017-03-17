#Scraping d'adresse mails Google
library(httr)
library(RSelenium)
library(rvest)

prox <- use_proxy(url = "proxy1",port = 8080,username = "ottavig", password = "Pierre-Fabre02")
agent <- user_agent("Mozilla/5.0 (compatible; Googlebot/2.1; +http://www.google.com/bot.html)")

q1 <- "https://www.google."
q2 <- "/search?hl="
q3 <-  "&num=100&q="

#Faire varier la marque

marques <-c("aderma","vichy","la+roche+posay","eucerin","uriage","bioderma","phyto","stiefel","mustela")

x <- c("by","ba","hr","ee","lv","lt","ge","kz","ro","si","rs","com.ua","fi","dk","no","se","nl","co.il","ae","com.bh","com.eg","jo","com.kw","com.sa","com.lb","com.om","com.qa","dz","co.ma","tn","com.ly","sn","ci","co.za")
y <- c("ru","bs","hrv","et","lv","lt","ka","ru","ro","sl","sr","uk","fi","da","no","sv","nl","he","ar","ar","ar","ar","ar","ar","ar","ar","ar","ar","ar","ar","ar","fr","fr","en")

#Afficher les 10 premiers r?sultats de Google
for (marque in marques){
for(i in 1:length(x)) {
q <- paste0(q1,x[i],q2,y[i],q3,marque)
fl_er <- try(html(q,prox,agent,timeout(4)),silent = T)
if(typeof(fl_er) == "externalptr"){
html_query <- html(q,prox,agent,timeout(4))

links <- html_query %>% html_nodes("h3.r a") %>% html_attr("href")
links <- substr(links,8,nchar(links))
if(length(links) < 10) links <- c(links,rep("",10-length(links)))
links <- c(x[i],links)
links <- links[1:11]
if(!exists("tab_fin")) tab_fin <- links else tab_fin <- cbind(tab_fin,links)
}
else
{
print(paste0("Marque ",marque," erreur site web ",x[i],"  ",y[i]))
}
Sys.sleep(10)
}
tab_fin <- data.frame(tab_fin)
assign(marque,tab_fin)
rm(tab_fin)
printf(paste0("Referencement Marque ",marque," termine"))
Sys.sleep(17)
}


library(RSelenium)

getChromeProfile("C:/Users/ottavig/AppData/Local/Google/Chrome/User Data","Default")
 
# make sure you have the server
checkForServer()

# use default server 
startServer()

startServer(invisible = FALSE, port)
remDr <- remoteDriver(browserName = "chrome", port = 85)
remDr$open()

url<-"https://programs.iowadnr.gov/animalfeedingoperations/FacilitySearch.aspx?Page=0"
url <- "http://www.google.com"
remDr$open() #opens a browser
remDr$navigate(url)


# identify search button and click
searchID<-'//*[@id="ctl00_foPageContent_SearchButton"]'
webElem<-remDr$findElement(value = searchID)
webElem$clickElement()

# identify the table
tableID<-'//*[@id="ctl00_foPageContent_Panel1"]/div[2]/table'
webElem<-remDr$findElement(value = tableID)

doc<-htmlParse(remDr$getPageSource()[[1]])

tabledat<-readHTMLTable(doc)[[17]]
tabledat[,]<-lapply(tabledat[,],
                    function(x) gsub("Â?", "", as.character(x)))
tabledat<-tabledat[-nrow(tabledat),-1]
# go to next page
nextID<-'//*[@id="ctl00_foPageContent_FacilitySearchRepeater_ctl11_PagePlus1"]'
webElem<-remDr$findElement(value = nextID)
webElem$clickElement()
