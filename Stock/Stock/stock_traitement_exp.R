#Colonne 1 exp
#Charger les fichiers robot
#MODIFIER VALEUR DU CODE PAYS AUSTRALIE
path <- "O:/01-MAINTENANCE SI/ODS STOCK/stock_ods/robot_tra/"
code_pays <- c("989","960","957","999","998","997","995","996","991","992","990","985","971","978","970","974","982","980","969","965","981","956","955","946","953","964","845")
filiale <-   c("006","010","014","024","025","026","027","029","030","034","049","053","061","064","066","067","070","071","073","079","081","301","303","305","306","308","312")

robot_file <- list.files(path)
robot_file <- robot_file[regexpr("xls$",robot_file,perl = T)!= -1]
if (length(robot_file) != 0) {
  
#Attribuer le bon nom au fichier
wr_files <- files_exp[nchar(files_exp) != 7]
for(file in wr_files) {
  df <- get(file)
  ind1 <- names(sort(table(df$V3)[1]))
  new_file <- paste0("F",filiale[which(code_pays == ind1)],"exp")
  assign(new_file,df)
  files_exp <- c(files_exp[-which(files_exp == file)],new_file)
  rm(list = file)
}

#nettoyer les fichiers
for(file in files_exp) {
  df <- get(file)
  rm_ind <- which(is.na(df$V16) | nchar(df$V16) != 6)
  if(length(rm_ind) != 0) df <- df[-rm_ind,]
  assign(file,df)
}


#Mettre le bon nombre de colonnes
for(file in files_exp) {
  df <- get(file)
  ind1 <- 25 - length(df)
  if (ind1 > 0) for (i in 1:ind1) df[,paste0("V",26-i)] <- ""
  if (ind1 < 0) df <- df[,1:25]
  assign(file,df)
}


#Comparer le fichier transmit & envoye
for(file in files_exp) {
  df <- get(file)
  path_fil <- paste0(path,"wrld_exp_",substr(file,2,4),".xls")
  robot <- try(read.table(colClasses= "character",path_fil, sep= "\t",head =T))
  if(attr(robot,"class") != "try-error") {
  names(robot) <- paste0("V",1:25)
  id_robot <- paste0(robot$V8,robot$V16)
  id_tra <- paste0(df$V8,df$V16)
  id <- which(!is.na(match(id_robot,id_tra)))
  df_final <- robot[id,]
  #Ajouter les lignes des mois pricendet s'il y en a
  id2 <- which(is.na(match(id_tra,id_robot)))
  if(length(id2) > 0) df_final <- rbind(df_final,df[id2,])
  } else {print(paste0("Fichier Transit ",file," Inexistant"));df_final <- df}
  assign(file,df_final)
}

rm(df_final,ind1,id,id2,id_robot)

#Virer les enregistrement type 0.
for(file in files_exp) {
  df <- get(file)
  df[,6] <- gsub("0\\.","0",df[,6],perl = T)
  assign(file,df)
}

#ENTETE STOCK
for(file in files_exp) {
  df <- get(file)
  names(df) <- test <- c("Code client","Nom client","Code pays","lib pays","n0 facture-invoice","container","type etat","N0 commande-N0 order","date facture-invoice date","refcde","conditions de paiement-terms of payment","Echeance le-payment date","Moyen de transport-transport id","Devise-invoicing currency","n0 ligne-n0 line","Code article-item number","Designation","Quantiti commandie-order qty","Quantiti facturie-Invoice qty","manquant-missing qty","Prix transfert-coggs","Montant ligne-line amount","Product type","Marque-Brand","Pays livraison")
  assign(file,df)
}
} else print("FICHIERS DES EXPEDITIONS MANQUANTS")

rm_ind <- which(regexpr("(F[0-9][0-9][0-9]exp|F[0-9][0-9][0-9]stk|files)",ls(),perl=T) == -1)
rm(list = ls()[rm_ind],rm_ind,V8)