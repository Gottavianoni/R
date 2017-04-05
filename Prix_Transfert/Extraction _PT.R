library(RODBC)
rov <- odbcConnect(dsn = "*****",uid = "*****",pwd = "*****")

dt1 <- "{ts '2012-01-01 00:00:00'}"
dt2 <- "{ts '2013-01-01 00:00:00'}"

query <- paste0("SELECT * 
                FROM ADMREF.T_ART_ENT_PRIX_REF T_ART_ENT_PRIX_REF 
                WHERE (T_ART_ENT_PRIX_REF.COENT='999') 
                AND (T_ART_ENT_PRIX_REF.DTEFT>=",dt1,"And T_ART_ENT_PRIX_REF.DTEFT<",dt2,") 
                OR (T_ART_ENT_PRIX_REF.COENT='992') 
                AND (T_ART_ENT_PRIX_REF.DTEFT>=",dt1," 
                And T_ART_ENT_PRIX_REF.DTEFT<",dt2,") 
                OR (T_ART_ENT_PRIX_REF.COENT='982') AND (T_ART_ENT_PRIX_REF.DTEFT>=",dt1," 
                And T_ART_ENT_PRIX_REF.DTEFT<",dt2,")")


df12 <- sqlQuery(rov,query)
df13 <- sqlQuery(rov,query)
df14 <- sqlQuery(rov,query)
df15 <- sqlQuery(rov,query)
df16 <- sqlQuery(rov,query)


PT <- df16
PT <- df15
PT <- df14
PT <- df13
PT <- df12

PT$DTEFT <- gsub("-","",as.character(PT$DTEFT))
#PT$DTEFT <- paste0(substr(PT$DTEFT,7,10),substr(PT$DTEFT,4,5),substr(PT$DTEFT,1,2),substr(PT$DTEFT,12,13),substr(PT$DTEFT,15,16))
PT$clc <- paste0(PT$COENT,PT$COARTITN)

ref <- unique(df16$clc)
ref <- unique(df$clc)

ind <- which(is.na(match(PT$clc,ref)))
PT <- PT[ind,]
clc <- unique(PT$clc)


for (code in clc){
  ind <- which(PT$clc == code)
  if(length(ind) > 1) {
    indmax <- which(PT$DTEFT[ind] == max(PT$DTEFT[ind]))[1]
    PT <- PT[-ind[-indmax],]
  }
  print(paste0(which(code == clc)," sur ",length(clc)))
}


df12 <- PT
df13 <- PT
df14 <- PT
df15 <- PT
df16 <- PT

for (c in names(df16)[3:7]) df16[,c] <- as.character(df16[,c])
for (c in names(df15)[3:7]) df15[,c] <- as.character(df15[,c])
for (c in names(df14)[3:7]) df14[,c] <- as.character(df14[,c])
for (c in names(df13)[3:7]) df13[,c] <- as.character(df13[,c])
for (c in names(df12)[3:7]) df12[,c] <- as.character(df12[,c])

df <- merge(x = df16[,c("clc","VLPRX","VLPRXDEV")],y= df15[,c("clc","VLPRX","VLPRXDEV")],by.x = "clc", by.y = "clc" , all.x = T , all.y = T)
df <- merge(x = df, y = df14[,c("clc","VLPRX","VLPRXDEV")] , by.x = "clc", by.y = "clc" , all.x = T, all.y = T )
df <- merge(x = df, y = df13[,c("clc","VLPRX","VLPRXDEV")] , by.x = "clc", by.y = "clc" , all.x = T, all.y = T )
df <- merge(x = df, y = df12[,c("clc","VLPRX","VLPRXDEV")] , by.x = "clc", by.y = "clc" , all.x = T, all.y = T )

names(df)[2:3] <- c("VL16","VLDEV16")
names(df)[4:5] <- c("VL15","VLDEV15")
names(df)[6:7] <- c("VL14","VLDEV14")
names(df)[8:9] <- c("VL13","VLDEV13")
names(df)[10:11] <- c("VL12","VLDEV12")


df$PT <- ifelse(!is.na(df$VL12),df$VL12,"NR")
df$PT <- ifelse(!is.na(df$VL13),df$VL13,df$PT)
df$PT <- ifelse(!is.na(df$VL14),df$VL14,df$PT)
df$PT <- ifelse(!is.na(df$VL15),df$VL15,df$PT)
df$PT <- ifelse(!is.na(df$VL16),df$VL16,df$PT)

df$PTDEV <- ifelse(!is.na(df$VLDEV12),df$VLDEV12,"NR")
df$PTDEV <- ifelse(!is.na(df$VLDEV13),df$VLDEV13,df$PTDEV)
df$PTDEV <- ifelse(!is.na(df$VLDEV14),df$VLDEV14,df$PTDEV)
df$PTDEV <- ifelse(!is.na(df$VLDEV15),df$VLDEV15,df$PTDEV)
df$PTDEV <- ifelse(!is.na(df$VLDEV16),df$VLDEV16,df$PTDEV)

df <- df[,c("clc","PT","PTDEV")]


dwh <- odbcConnect("DWH_PROD2_HOLDING",uid="utlboe", pwd = "boeutl")

  query <- paste0("SELECT *
  FROM ADMDWH.R_TCO_DWH
  WHERE R_TCO_DWH.COENT in ('999','992','982')
  AND FLOUV = 'O'")

tco <- sqlQuery(dwh,query)

tco <- tco[,c("COARTITN","COENT")]
tco$id <- paste0(tco$COENT,tco$COARTITN)

tco <- merge(tco,df,by.x= "id",by.y = "clc",all.x=T)
