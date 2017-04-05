#ANALYSE DES RUPTURES

library(RODBC)
dwh <- odbcConnect(dsn = "*****",uid = "*****",pwd = "*****")
ref <- odbcConnect(dsn = "*****",uid = "*****",pwd = "*****")

query_1 <- "SELECT * 
FROM D_RUPTURE_DWH"
rup <- sqlQuery(dwh,query_1)

query_1 <- "SELECT * 
FROM R_CAUSES_RUPTURE_DWH"
cs <- sqlQuery(dwh,query_1)
  
query_1 <- "SELECT * 
FROM R_SOUS_CAUSES_RUPTURE_DWH"
scs <- sqlQuery(dwh,query_1)

query_1 <- "SELECT * 
FROM R_HIEART_DWH"
hie <- sqlQuery(dwh,query_1)

query_1 <- "SELECT COART,COMDL 
FROM R_ARTICLE_DWH"
hie <- sqlQuery(dwh,query_1)

query_1 <- "SELECT COARTITN,COMDL 
FROM T_ARTICLE_REF"
art <- sqlQuery(ref,query_1)

cs <- scs[,c("COSOUCAURUP","COCAURUP","LBSOUCAURUP",'LBCAURUP')]
art <- hie[,c("COART","COGAM","COMAR","LBGAM","LBMAR","COCLSTHE","LBCLSTHE")]

rup <- merge(rup, cs,by.x= "COSOUCAURUP",by.y= "COSOUCAURUP",all.x =T)
rup <- merge(rup,art,by.x = "COART",by.y="COART",all.x =T)

wrld_stkn_061 <- merge(wrld_stkn_061,hie,by.x = "V3",by.y = "COART",all.x = T)
wrld_stkn_061 <- merge(wrld_stkn_061,art,by.x = "V3",by.y = "COARTITN",all.x = T)

View(tapply(X = wrld_stkn_061$V10,INDEX = wrld_stkn_061$COMDL,sum))
View(tapply(X = wrld_stkn_061$V10,INDEX = wrld_stkn_061$COMDL.y,sum))

x <- data.frame(table(as.character(rup$LBGAM),as.character(rup$COSOUCAURUP)))
