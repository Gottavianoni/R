#ANALYSE et GENERATION D'UN RAPPORT AUTOMATIQUE pour les fichiers STK
#args <- commandArgs(TRUE)
fichiers <- list.files("C:/Users/ottavig/Desktop/test/")
list_fil <- c("006","010","014","024","025","026","027","029","030","034","049","053","061","064","066","067","070","071","073","081","301","303","305","306","308")
fichier <- fichiers[regexpr("\\.",fichiers) != -1]
path <- "C:/Users/ottavig/Desktop/test/"

fichier <- fichiers[2]

#G?n?ration de la trame du rapport
rapport_final <- data.frame(cbind(AXE  = c("1-EXT","1-NAME","1-FIL","2-DATE","3-NBCOL","3-HEAD",
                        "3-END","3-DBL","4-WDATE","4-LCOART","4-WFIL","4-ENC","4-BLKSEP"),MARK = rep(0,13)),COMMENTARY = rep(0,13))
for(c in names(rapport_final)) rapport_final[,c] <- as.character(rapport_final[,c])
for(c in names(rapport_final)[c(2,3)]) rapport_final[,c] <- as.numeric(rapport_final[,c])

#determiner l'extension du fichier
extension <- substr(fichier,regexpr("(\\.xls|\\.csv|\\.txt)",fichier,perl=T)+1,nchar(fichier))
if(!extension %in% c("xls","xlsx","csv","txt")) rapport_final[1,"MARK"] <- rapport_final[1,"MARK"] +1
if(extension != "txt") rapport_final[1,"MARK"] <- rapport_final[1,"MARK"] + 1

#determiner le nom du fichier
name <- substr(fichier,1,regexpr("\\.",fichier,perl=T)-1)
if(regexpr("wrld_stkn_",name,fixed=T) == -1) rapport_final[2,"MARK"] <- rapport_final[2,"MARK"] + 1

#d?termine la filiale du fichier
filiale <- substr(fichier,regexpr("\\.",fichier,perl=T)-3,regexpr("\\.",fichier,perl=T)-1)
if(!filiale %in% list_fil) { rapport_final[3,"MARK"] <- rapport_final[3,"MARK"] + 1;flag_f <- "OK";}
names(rapport_final)[1] <- name

#Determine si le nombre de colonne est bon
source("C:/Users/ottavig/Documents/Script_R/Stock/stock_rapport_lecture_fichier.R")
#Calcul du NB Col
df <- get(name)
if(length(names(df)) != 17) rapport_final[5,"MARK"] <- rapport_final[5,"MARK"] + 1
if(!length(names(df)) %in% c(16,17,18)) rapport_final[5,"MARK"] <- rapport_final[5,"MARK"] + 1

#Calcul des ent?tes
if(regexpr("[1-9][1-9][1-9]",paste(df[1,],collapse=""),perl=T) == -1 ){ df <- df[-1,]; rapport_final[6,"MARK"] <- rapport_final[6,"MARK"] + 1;}
if(regexpr("[1-9][1-9][1-9]",paste(df[1,],collapse=""),perl=T) == -1 ){ df <- df[-1,]; rapport_final[6,"MARK"] <- rapport_final[6,"MARK"] + 1;}
if(regexpr("[1-9][1-9][1-9]",paste(df[1,],collapse=""),perl=T) == -1 ){ df <- df[-1,]; rapport_final[6,"MARK"] <- rapport_final[6,"MARK"] + 1;}
if(regexpr("[1-9][1-9][1-9]",paste(df[1,],collapse=""),perl=T) == -1 ){ df <- df[-1,]; rapport_final[6,"MARK"] <- rapport_final[6,"MARK"] + 1;}
if(regexpr("[1-9][1-9][1-9]",paste(df[1,],collapse=""),perl=T) == -1 ){ df <- df[-1,]; rapport_final[6,"MARK"] <- rapport_final[6,"MARK"] + 1;}

#Calcul des lignes ab?rantes de fin 
ind <- length(df[,1])
if(regexpr("[1-9][1-9][1-9]",paste(df[ind,],collapse=""),perl=T) == -1 ){ df <- df[-ind,]; rapport_final[7,"MARK"] <- rapport_final[7,"MARK"] + 1;}
ind <- length(df[,1])
if(regexpr("[1-9][1-9][1-9]",paste(df[ind,],collapse=""),perl=T) == -1 ){ df <- df[-ind,]; rapport_final[7,"MARK"] <- rapport_final[7,"MARK"] + 1;}
ind <- length(df[,1])
if(regexpr("[1-9][1-9][1-9]",paste(df[ind,],collapse=""),perl=T) == -1 ){ df <- df[-ind,]; rapport_final[7,"MARK"] <- rapport_final[7,"MARK"] + 1;}
ind <- length(df[,1])
if(regexpr("[1-9][1-9][1-9]",paste(df[ind,],collapse=""),perl=T) == -1 ){ df <- df[-ind,]; rapport_final[7,"MARK"] <- rapport_final[7,"MARK"] + 1;}
ind <- length(df[,1])
if(regexpr("[1-9][1-9][1-9]",paste(df[ind,],collapse=""),perl=T) == -1 ){ df <- df[-ind,]; rapport_final[7,"MARK"] <- rapport_final[7,"MARK"] + 1;}
rm(ind)

#D?tection des doublons
ln1 <- length(df[,1])
udf <- unique(df)
ln2 <- length(udf[,1])
if (ln1 !=ln2) rapport_final[8,"MARK"] <- rapport_final[8,"MARK"] + 1
rm(ln1,ln2,udf)

#CHECK DU CODE FILIALE le code est bon et qu'il n'y a pas d'autre code filiale
if(!names(sort(table(df$V1)[],decreasing = T)[1]) %in% list_fil) rapport_final[11,"MARK"] <- rapport_final[11,"MARK"] + 1
if(!is.na(names(sort(table(df$V1)[],decreasing = T)[2]))) rapport_final[11,"MARK"] <- rapport_final[11,"MARK"] + 1

#CODE DATE
if(names(sort(table(df$V2)[],decreasing = T)[1]) != paste0(substr(Sys.Date(),1,4),substr(Sys.Date(),6,7)) ) rapport_final[9,"MARK"] <- rapport_final[9,"MARK"] + 1
if(!is.na(names(sort(table(df$V2)[],decreasing = T)[2]))) rapport_final[9,"MARK"] <- rapport_final[9,"MARK"] + 1

#CODE ARTICLE
if(length(which(nchar(df$V3) != 6)) != 0) rapport_final[10,"MARK"] <- rapport_final[10,"MARK"] + 1

#CHECK LES ESPACES DANS LES NOMBRES
for (c in c("V6","V7","V8","V10","V11","V12","V13","V14","V15","V17")) {
  if(length(which(regexpr(" ",df[,c]) != -1)) > 0) rapport_final[13,"MARK"] <- 1
}

#CHECK DU RATIO DE POINTS ET DE VIRGULES
  nb_pt <- length(which(regexpr("\\.",df$V6,perl=T) != -1))+length(which(regexpr("\\.",df$V7,perl=T) != -1))+length(which(regexpr("\\.",df$V8,perl=T) != -1))
  nb_vir <- length(which(regexpr(",",df$V6,perl=T) != -1 ))+length(which(regexpr(",",df$V7,perl=T) != -1 ))+length(which(regexpr(",",df$V8,perl=T) != -1 ))
  
  #Si il n'y a pas de point et qu'il y a des virgules on converti la virgule en point
  if(nb_pt == 0 & nb_vir !=0 ) rapport_final[13,"MARK"] <-  rapport_final[13,"MARK"] + 1
  
  #Si il y a les 2 on d?tecte la meilleure somme
  if(nb_pt != 0 & nb_vir != 0) rapport_final[13,"MARK"] <-  rapport_final[13,"MARK"] + 2
  

#DELAIS
#print(paste0("Quand le fichier de la Filiale ",filiale," a il ete envoye au Siege ?"))
#print("0 -> J a J+2 | 1 -> J+3 | 2 -> J+4 ")
#i = ""
#while( i != "end"){
#flag_dte  <- scan(nmax = 1)
#if(flag_dte[length(flag_dte)] %in% c(0,1,2)){ i <- "end"; rapport_final[4,"MARK"] <- flag_dte;}
#}
