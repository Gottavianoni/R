#PROGRAMME DE RECUPERATION DES INFOS MEMBRES SITE BEAUTE-TEST.COM
#pers_desc <- read.csv2("~/Stats/Sources/consmmateurs.csv")
#avis_cons <- read.csv2("~/Stats/Sources/avis_conso.csv")

#departements <- read.csv("~/Stats/Sources/departements.csv", sep=";", stringsAsFactors=FALSE)
#pers_desc$sexe <- substr(pers_desc$desc,1,1)
#pers_desc$date_ins <- substr(pers_desc$desc,22,30)
#pers_desc$dep <- substr(pers_desc$desc,as.numeric(gregexpr( "(",pers_desc$desc,fixed = T)) + 1, as.numeric(gregexpr( ")",pers_desc$desc,fixed = T)) -1 )
#pers_desc$peau<- substr(pers_desc$caract,as.numeric(gregexpr( "Type de peau : ",pers_desc$caract,fixed = T)) + 15,as.numeric(gregexpr( "shydratation",pers_desc$caract,fixed = T)) - 4)
#pers_desc$desyd<- substr(pers_desc$caract,as.numeric(gregexpr( "hydratation : ",pers_desc$caract,fixed = T)) + 14,as.numeric(gregexpr( "Type de cheveux",pers_desc$caract,fixed = T)) - 1)
#pers_desc$cheveux <- substr(pers_desc$caract,as.numeric(gregexpr( "Type de cheveux : ",pers_desc$caract,fixed = T)) + 18,as.numeric(gregexpr( "Type de cheveux : ",pers_desc$caract,fixed = T)) + 50)
#pers_desc$dep <- gsub("é","e",pers_desc$dep)
#pers_desc$dep <- gsub("ô","o",pers_desc$dep)
#pers_desc$dep <- gsub("è","e",pers_desc$dep)
#pers_desc$dep <- toupper(pers_desc$dep)
#pers_desc <- pers_desc[,c(1,4:9)]
#pers_desc <- merge(pers_desc,departements[,c("CODE_DEPT","NOM_DEPT")], all.x = T, by.x = "dep",by.y = "NOM_DEPT")

#pers_desc$peau <- gsub("é","e",pers_desc$peau)
#pers_desc$peau <- gsub("ô","o",pers_desc$peau)
#pers_desc$peau <- gsub("è","e",pers_desc$peau)

#pers_desc$cheveux <- gsub("é","e",pers_desc$cheveux)
#pers_desc$cheveux <- gsub("ô","o",pers_desc$cheveux)
#pers_desc$cheveux <- gsub("è","e",pers_desc$cheveux)
#rm(departements)


