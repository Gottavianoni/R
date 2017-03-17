#Colonne 1 stk
for( file in files_stk) {
  df <- get(file)
  ind1 <- names(sort(table(df$V1)[1]))

  #affecter la valeur dominante du code filiale
  df$V1 <- ifelse(!is.na(table(df$V1)[2]),ind1,df$V1)
  
  #Formater le code filliale
  ind1 <- ifelse(nchar(ind1) == 2 && regexpr("[0-9][0-9]",ind1,perl=T) != -1,paste0("0",ind1),ind1)
  ind1 <- ifelse(nchar(ind1) == 1 && regexpr("[0-9]",ind1,perl=T) != -1,paste0("00",ind1),ind1)
  df$V1 <- ind1
  
  #checker que le code filiale existe et correspond ` la colonne 1, si il existe pas mettre celui du nom
  ls_fil <-   c("006","010","014","024","025","026","027","029","030","034","049","053","061","064","066","067","070","071","073","079","081","301","303","305","306","308","311","312")
  if(substr(file,2,4) != ind1) {
    if(length(which(regexpr(ind1,ls_fil,fixed=T) != -1)) == 0){df$V1 <- substr(file,2,4);assign(file,df);}
    #faire un rm du fichier avec mauvais nom
    if(length(which(regexpr(substr(file,2,4),ls_fil,fixed=T) != -1)) == 0) {df$V1 <- ind1; assign(paste0("F",ind1,"stk"),df);files_stk <- c(files_stk,paste0("F",ind1,"stk"));files_stk <- files_stk[files_stk != file];rm(list = file)} 
  }
  else 
  {  
    df$V1 <- ind1
    assign(file,df)
  }
}

#Colonne 2 stk - date egale a M-1
for( file in files_stk) {
  df <- get(file)
  ind1 <- names(sort(table(df$V2),decreasing = T)[1])
  val <- ifelse(substr(Sys.Date(),6,7) == "01",as.character(as.numeric(paste0(as.numeric(substr(Sys.Date(),1,4))-1,12))),as.character(as.numeric(paste0(substr(Sys.Date(),1,4),substr(Sys.Date(),6,7))) - 1))
  if (regexpr("201[0-9][0-9][0-9]",ind1) == -1) ind1 <- val
  df$V2 <- ind1
 assign(file,df)
}

#Rectifier les codes articles qui ne sont pas a 6 caracteres V3
for(file in files_stk){
df <- get(file)
df$V3 <- gsub("\\s","",df$V3)
df$V3 <- gsub("\\t","",df$V3)
df$V3 <- gsub("\\r","",df$V3)
df$V3 <- gsub("\\n","",df$V3)
df$V3 <- ifelse(6-nchar(df$V3) == 1,paste0("0",df$V3),df$V3)
df$V3 <- ifelse(6-nchar(df$V3) == 2,paste0("00",df$V3),df$V3)
df$V3 <- ifelse(6-nchar(df$V3) == 3,paste0("000",df$V3),df$V3)
df$V3 <- ifelse(6-nchar(df$V3) == 4,paste0("0000",df$V3),df$V3)
df$V3 <- ifelse(6-nchar(df$V3) == 5,paste0("00000",df$V3),df$V3)
df$V3 <- ifelse(regexpr("[0-9][0-9][0-9][0-9][0-9][0-9]",df$V3,perl=T) == -1,"000DEF",df$V3)
df$V3 <- ifelse(nchar(df$V3) > 6,substr(df$V3,1,6),df$V3)
assign(file,df)
}

#Colonne 4 stk - code non remplis pas de modif

for(file in files_stk){
df <- get(file)
df$V4 <- gsub("\\s","",df$V4)
df$V4 <- gsub("\\t","",df$V4)
df$V4 <- gsub("\\r","",df$V4)
df$V4 <- gsub("\\n","",df$V4)
if(nchar(df$V4) > 20) df$V4 <- substr(dfV4,1,20)
df$V4 <- ifelse(df$V4 == "",df$V3,df$V4)
df$V4 <- ifelse(is.na(df$V4),df$V3,df$V4)
assign(file,df)
}

#Ajouter une colonne vide pour les stock qui n'ont pas 17 lignes stk
for(file in files_stk){
  df <- get(file)
  if (length(names(df)) < 17) {
    ind_col <- 17 - length(names(df))
    for ( i in (18 - ind_col):17) df[,paste0("V",i)] <- "0.000"
  }
  assign(file,df)
}

#Colonne 5
for(file in files_stk){
df <- get(file)
for (i in 1:20) df$V5 <- gsub("  "," ",df$V5,fixed= T)
df$V5 <- ifelse(nchar(df$V5) > 30,substr(df$V5,1,20),df$V5)
df$V5 <- tolower(df$V5)
df$V5 <- gsub("\\{","u",df$V5)
df$V5 <- gsub("\\|","u",df$V5)
df$V5 <- gsub("\\`","a",df$V5)
df$V5 <- toupper(df$V5)
assign(file,df)
}

#Supprimer les colonnes en trop  stk
for(file in files_stk){
  df <- get(file)
  df <- df[,c(1:17)]
  assign(file,df)
}

#Rectifier les lignes NA's
for(file in files_stk){
  df <- get(file)
  for (c in c("V6","V7","V8","V10","V11","V12","V13","V14","V15","V17")){
  df[,c] <- ifelse(regexpr("[0-9]",df[,c],perl=T) == -1, "0",df[,c])
  df[,c] <- ifelse(is.na(df[,c]),"0",df[,c])
  }
  assign(file,df)   
}

#Supprimer les espace separateur miliers et uniformiser les virgules stk
for(file in files_stk) {
  
  df <- get(file)
  for (c in c("V6","V7","V8","V10","V11","V12","V13","V14","V15","V17")) df[,c] <- gsub(" ","",df[,c],fixed=T)
  
  nb_pt <- length(which(regexpr(".",df$V8,fixed=T) != -1))
  nb_pt <- nb_pt + length(which(regexpr(".",df$V10,fixed=T) != -1))
  nb_vir <- length(which(regexpr(",",df$V8,fixed=T) != -1 ))
  nb_vir <- nb_vir + length(which(regexpr(",",df$V10,fixed=T) != -1 ))
  
  #Si il n'y a pas de point et qu'il y a des virgules on converti la virgule en point
  if(nb_pt == 0 & nb_vir !=0 ) {
    for (c in c("V6","V7","V8","V10","V11","V12","V13","V14","V15","V17")) df[,c] <- gsub("\\,","\\.",df[,c],perl=T)
    if (length(which(regexpr("\\.[0-9][0-9][0-9]\\.",df$V8) != -1)) != 0) for (c in c("V6","V7","V8","V10","V11","V12","V13","V14","V15","V17")) df[,c] <- gsub("\\.","",df[,c],perl=T)
  }
  
  
  #Si il y a les 2 on ditecte la meilleure somme
  if(nb_pt != 0 & nb_vir != 0)  {
    
    V6 <- gsub("\\,","",df$V6,perl=T)
    V7 <- gsub("\\,","",df$V7,perl=T)
    V8 <- gsub("\\,","",df$V8,perl=T)
    V10 <- gsub("\\,","",df$V10,perl=T)
    total1 <- length(which(round(as.numeric(V6) * as.numeric(V7)) == round(as.numeric(V8)))) / length(V6)
    
    V6 <- gsub("\\.","",df$V6,perl=T)
    V7 <- gsub("\\.","",df$V7,perl=T)
    V8 <- gsub("\\.","",df$V8,perl=T)
    V10 <- gsub("\\.","",df$V10,perl=T)
    total2 <- length(which(round(as.numeric(gsub(",","\\.",V6)) * as.numeric(gsub(",","\\.",V7))) == round(as.numeric(gsub(",","\\.",V8))))) /  length(V6)
    
    
    if (total1 >= total2) {
      for (c in c("V6","V7","V8","V10","V11","V12","V13","V14","V15","V16","V17")) df[,c] <- gsub("\\,","",df[,c],perl=T)
    }
    
    if (total2 > total1) {
      for (c in c("V6","V7","V8","V10","V11","V12","V13","V14","V15","V16","V17")) {
        df[,c] <- gsub("\\.","",df[,c],perl=T)
        df[,c] <- gsub("\\,","\\.",df[,c],perl=T)
      }
    }
  }
  assign(file,df)
}

#AGGREGER les meme code articles
for(file in files_stk){
  df <- get(file)
  print(file)
  for (c in c("V6","V7","V8","V10","V11","V12","V13","V14","V15","V16","V17")) { df[,c] <- as.numeric(df[,c])}
  for (c in c(1:5,7,9)) { df[,c] <- ifelse(is.na(df[,c]),"",df[,c])}
  if(length(unique(paste0(df$V1,df$V2,df$V3,df$V7,df$V8))) !=  length(unique(paste0(df$V1,df$V2,df$V3)))){
  df1 <- aggregate(x = df[,c(6,8,10:17)] , by = df[,c(1:3)], FUN = sum)
  df2 <- aggregate(x = df[,c(4:5,9)] , by = df[,c(1:3)], FUN = paste0)
  chaain <- NULL
  chaain2 <- NULL
  chaain3 <- NULL
  for (i in 1:length(df2$V4)) chaain <- c(chaain,df2$V4[[i]][[1]])
  for (i in 1:length(df2$V5)) chaain2 <- c(chaain2,df2$V5[[i]][[1]])
  for (i in 1:length(df2$V9)) chaain3 <- c(chaain3,df2$V9[[i]][[1]])
  df1$V7 <- df1$V8/df1$V6
  df1$V4 <- chaain
  df1$V5 <- chaain2
  df1$V9 <- chaain3
  df <- df1[,paste0("V",1:17)]
  }
  assign(file,df)
}


#FORMATER LE NOMBRES en --,000--
for(file in files_stk){
  df <- get(file)
  df$V14 <- ifelse(substr(df$V14,1,1) != "-",paste0("-",df$V14),df$V14)
  for (c in c("V6","V7","V8","V10","V11","V12","V13","V14","V15","V16","V17")) {
    df[,c] <- ifelse(regexpr("[0-9]",df[,c],perl = T) == -1,"0,0000",round(as.numeric(df[,c]),4))
    df[,c] <- ifelse(is.na(df[,c]),"0,0000",df[,c])
    df[,c] <- ifelse(df[,c] == "0","0,0000",df[,c])
    df[,c] <- gsub("\\.","\\,",df[,c],perl = T)
    df[,c] <- ifelse(regexpr("\\,",df[,c],perl=T) == -1,paste0(df[,c],",0000"),df[,c])
    df[,c] <- ifelse(regexpr("\\,[0-9]",df[,c],perl=T) == -1,paste0(df[,c],"0000"),df[,c])
    df[,c] <- ifelse(regexpr("\\,[0-9][0-9]",df[,c],perl=T) == -1,paste0(df[,c],"000"),df[,c])
    df[,c] <- ifelse(regexpr("\\,[0-9][0-9][0-9]",df[,c],perl=T) == -1,paste0(df[,c],"00"),df[,c])
    df[,c] <- ifelse(regexpr("\\,[0-9][0-9][0-9][0-9]",df[,c],perl=T) == -1,paste0(df[,c],"0"),df[,c])
    }
  assign(file,df)
}

#Formater la colonne 14
for(file in files_stk){
  df <- get(file)
  df$V14 <- ifelse(df$V14 == "0,0000","-0,0000",df$V14)
  assign(file,df)
}


#TRONQUER LE TYPE DE PRODUIT
for(file in files_stk){
  df <- get(file)
  df$V9 <- substr(df$V9,1,3)
  df$V9 <- ifelse(toupper(df$V9) %in% c("TRI","FG","ECH","FGD","GIF","KIT","OTH","POS","PF","ECH","SAM","DOC","SA"),toupper(df$V9),"0")
  assign(file,df)
}



#Virer les doublons
for(file in files_stk){
  df <- get(file)
  df <- unique(df)
  assign(file,df)
}


#TRIER PAR CODE ARTICLE
for(file in files_stk){
  df <- get(file)
  df <- df[order(df$V3),]
  assign(file,df)
}

rm_ind <- which(regexpr("(exp$|stk$|files)",ls(),perl=T) == -1)
rm(list = ls()[rm_ind])
rm(v8)
