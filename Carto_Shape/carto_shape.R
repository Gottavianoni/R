####SCRIPT DE RECUPERATION DE CARREFOUR
dep <- read.csv2("C:/Users/ottavig/Desktop/car_lafay_extr/Cartes/villes_france_dep.csv",header = F)
names(dep) <- c("DEP","VILLE")
dep <- dep[-which(dep$DEP != "59" & dep$VILLE == "BAILLEUL"),]
dep <- dep[-which(dep$DEP != "53" & dep$VILLE == "LAVAL"),]
dep <- dep[-which(dep$DEP != "26" & dep$VILLE == "VALENCE"),]

lafayette <- read.csv2("//varan/Echange/Web_Extractor/AVENE_FR/Extraction_Full/Lafayette/lafayette_20160823_1100.csv")
lafayette$site <-toupper(lafayette$site)

lafayette$site <- gsub("CANNES LA BOCCA","CANNES",lafayette$site,fixed = T)
lafayette$site <- gsub("PUY-EN-VELAY","LE PUY-EN-VELAY",lafayette$site,fixed = T)
lafayette$site <- gsub("\\s","\\-",lafayette$site)
lafayette$site <- gsub("LE-PUY-EN-VELAY","LE PUY-EN-VELAY",lafayette$site,fixed = T)
lafayette$site <- gsub("LE-MANS","LE MANS",lafayette$site,fixed = T)

lafayette <- merge(lafayette,dep,by.x = "site",by.y="VILLE",all.x = T)
table(lafayette[is.na(lafayette$DEP),"site"])

lafayette_eta <- lafayette[lafayette$name == "AVENE Avène Cicalfate Crème 40 Ml",]

list_doublon <- unique(as.character(lafayette_eta$DEP))
for (ville_d in list_doublon) {
  if(length(which(lafayette_eta$DEP == ville_d)) > 1) {
    ind2 <- which(lafayette_eta[lafayette_eta$DEP == ville_d,"prix"] != min(lafayette_eta[lafayette_eta$DEP == ville_d,"prix"]))
    if(length(ind2) == 0) ind2 <- 1
    lafayette_eta <- lafayette_eta[-which(lafayette_eta$DEP == ville_d)[ind2],]
  }
  print( paste0(length(lafayette_eta[,1])," - " ,ville_d))
}

all_dep <- data.frame(DEP = unique(as.character(dep$DEP)))
all_dep <- merge(all_dep,lafayette_eta, by.x = "DEP", by.y="DEP",all.x =T)
all_dep[is.na(all_dep$prix),"prix"] <- median(lafayette_eta$prix)

library(scales)
library(rgeos)
library(maptools)
map_dep <- readShapeSpatial("C:/Users/ottavig/Documents/GIS/DEP/DEPARTEMENT.SHP")

library(ggplot2)
#map_dep_for <- 
x <- ggplot(all_dep, aes(map_id = DEP)) +
  geom_map(aes(fill = prix), map = map_dep_for) +
  expand_limits(x = map_dep_for$long, y = map_dep_for$lat) +
  scale_fill_gradient2(low = muted("red"),
                       mid = "grey98", midpoint = median(lafayette_eta$prix) , high = muted("blue"), limits = c(min(lafayette_eta$prix), max(lafayette_eta$prix))) +
  theme_void()