#MISE EN FORME
date <- as.character(Sys.Date())
heure <- paste0(as.character(substr(Sys.time(),12,13)),":00")

for(c in names(df)) df[,c] <- as.character(df[,c])
df <- unique(df)
df <- df[!is.na(df$NOM),]

#NOM PRODUIT
df$NOM <- gsub("\\s{2,}"," ",df$NOM)
df$NOM <- gsub("\n","",df$NOM)
df$NOM <- gsub("\\n","",df$NOM)
df$NOM <- gsub("\t","",df$NOM)
df$NOM <- gsub("\\t","",df$NOM)
df$NOM <- gsub("\r","",df$NOM)
df$NOM <- gsub("\\r","",df$NOM)
df$NOM <- gsub("è","e",df$NOM)
df$NOM <- gsub("é","e",df$NOM)
df$NOM <- gsub("ê","e",df$NOM)
df$NOM <- gsub("ë","e",df$NOM)
df$NOM <- gsub("à","a",df$NOM)
df$NOM <- gsub("ç","c",df$NOM)
df$NOM <- gsub("û","u",df$NOM)
df$NOM <- gsub("ü","u",df$NOM)
df$NOM <- gsub("î","i",df$NOM)
df$NOM <- gsub("ï","i",df$NOM)
df$NOM <- gsub("ô","o",df$NOM)
df$NOM <- gsub("ö","o",df$NOM)
df$NOM <- gsub("â","a",df$NOM)
df$NOM <- gsub("\\-"," ",df$NOM)
df$NOM <- gsub("\\_"," ",df$NOM)
df$NOM <- gsub("\\°","",df$NOM)
df$NOM <- gsub("\\|","",df$NOM)
df$NOM <- gsub("È","E",df$NOM,fixed = T)
df$NOM <- gsub("É","E",df$NOM,fixed = T)
df$NOM <- gsub("Ê","E",df$NOM,fixed = T)
df$NOM <- gsub("À","A",df$NOM,fixed = T)
df$NOM <- gsub("Â","A",df$NOM,fixed = T)
df$NOM <- gsub("Û","U",df$NOM,fixed = T)
df$NOM <- gsub("Ê","E",df$NOM,fixed = T)

df$NOM <- gsub("Ã¨","e",df$NOM,fixed = T)
df$NOM <- gsub("Â©","e",df$NOM,fixed = T)
df$NOM <- gsub("Ã©","e",df$NOM,fixed = T)
df$NOM <- gsub("Â\u2030","e",df$NOM,fixed = T)
df$NOM <- gsub("Ã\u2030","e",df$NOM,fixed = T)
df$NOM <- gsub("Ãˆ","e",df$NOM,fixed = T)
df$NOM <- gsub("Ã¨","e",df$NOM,fixed = T)
df$NOM <- gsub("Ã¯","i",df$NOM,fixed = T)
df$NOM <- gsub("Ã¯","i",df$NOM,fixed = T)
df$NOM <- gsub("Ã«","e",df$NOM,fixed = T)
df$NOM <- gsub("Ã©","e",df$NOM,fixed = T)
df$NOM <- gsub("Ã»","u",df$NOM,fixed = T)
df$NOM <- gsub("Ãª","e",df$NOM,fixed = T)
df$NOM <- gsub("Ã¨","e",df$NOM,fixed = T)
df$NOM <- gsub("Ã¢","a",df$NOM,fixed = T)
df$NOM <- gsub("Ã§","c",df$NOM,fixed = T)
df$NOM <- gsub("Ã®","i",df$NOM,fixed = T)
df$NOM <- gsub("ÃŠ","e",df$NOM,fixed = T)
df$NOM <- gsub("Ã¤","a",df$NOM,fixed = T)
df$NOM <- gsub("Ã\u20AC","a",df$NOM,fixed=T)
df$NOM <- gsub("Ã\u203A","U",df$NOM,fixed = T)
df$NOM <- gsub("AÃQ","AIQ",df$NOM,fixed = T)
df$NOM <- gsub("\\sÃ\\s"," a ",df$NOM)
df$NOM <- gsub("Ã³","o",df$NOM,fixed = T)
df$NOM <- gsub("Ãº","o",df$NOM,fixed = T)
df$NOM <- gsub("Ã\u0192A¨","e",df$NOM)
df$NOM <- gsub("Ã\u0192A©","e",df$NOM)
df$NOM <- gsub("Ã\u0191A¨","e",df$NOM)
df$NOM <- gsub("Ã\u0191A©","e",df$NOM)
df$NOM <- gsub("Ã\u0192","a",df$NOM)
df$NOM <- gsub("Ã¡","e",df$NOM)

df$NOM <- gsub("\\.","",df$NOM)
df$NOM <- gsub("\\-"," ",df$NOM)
df$NOM <- gsub("\\'"," ",df$NOM)
df$NOM <- gsub("A DERMA","ADERMA",df$NOM,fixed= T)

df$NOM <- gsub("\\(","",df$NOM)
df$NOM <- gsub("\\)","",df$NOM)
df$NOM <- gsub("\\[","",df$NOM)
df$NOM <- gsub("\\]","",df$NOM)

df$NOM <- gsub("\\s{2,}"," ",df$NOM)

df$NOM <- toupper(df$NOM)


write.csv2(df,paste0("C:/Users/ottavig/Desktop/PRODUIT_ASIN_FULL_",date,"-",gsub(":","",heure),".csv"),row.names = F)


protect <- read.csv2("C:/Users/ottavig/Desktop/protect.csv",colClasses = "character",header = F)

ind2 <- NULL
for (mot in protect$V1) {
  ind <- which(regexpr(mot,df$NOM) != -1)
  ind2 <- c(ind2,ind)
}

res <- unique(df[ind2,c(1,2,4)])
res <- res[substr(res$ASIN,1,1) == "B",]

write.csv2(res,paste0("C:/Users/ottavig/Desktop/PRODUIT_ASIN_UNIQUE_",date,"-",gsub(":","",heure),".csv"),row.names = F)
