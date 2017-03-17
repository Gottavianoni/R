#Type des fichiers
files <- ls()
files_stk <- ""
files_exp <- ""

#Determiner le type de fichiers
for(file in files){
  df <- get(file)
  if(length(df) > 24 & length(df) < 35){
    if( regexpr("F[0-9][0-9][0-9]exp|F[0-9][0-9][0-9]stk",file,perl=T) == -1 & regexpr("F[0-9][0-9][0-9]",file,perl=T) != -1) { assign(paste0(substr(file,1,4),"exp"),df);files_exp <- c(files_exp,paste0(substr(file,1,4),"exp"));rm(file)}
    else files_exp <- c(files_exp,file)
  }
  if(length(df) > 10 & length(df) <= 24){
    if( regexpr("F[0-9][0-9][0-9]exp|F[0-9][0-9][0-9]stk",file,perl=T) == -1 & regexpr("F[0-9][0-9][0-9]",file,perl=T) != -1) { assign(paste0(substr(file,1,4),"stk"),df);files_stk <- c(files_stk,paste0(substr(file,1,4),"stk"));rm(file)}
    else files_stk <- c(files_stk,file)
  }
}
files_exp <- files_exp[which(nchar(files_exp) != 0)]
files_stk <- files_stk[which(nchar(files_stk) != 0)]
files <- c(files_exp,files_stk)

#D?tecter les anomalies dans nom fichiers

#Virer les premiere ligne si entete
for( file in c(files_stk,files_exp)){
  df <- get(file)
  if(regexpr("[1-9][1-9][1-9]",paste(df[1,],collapse=""),perl=T) == -1 ) df <- df[-1,] 
  if(regexpr("[1-9][1-9][1-9]",paste(df[1,],collapse=""),perl=T) == -1 ) df <- df[-1,]
  if(regexpr("[1-9][1-9][1-9]",paste(df[1,],collapse=""),perl=T) == -1 ) df <- df[-1,]  
  if(regexpr("[1-9][1-9][1-9]",paste(df[1,],collapse=""),perl=T) == -1 ) df <- df[-1,] 
  if(regexpr("[1-9][1-9][1-9]",paste(df[1,],collapse=""),perl=T) == -1 ) df <- df[-1,]
  assign(file,df)
}

#Virer le mauvais encodage
for(file in files_stk) {
  df <- get(file)
  for (c in names(df)) df[,c] <- gsub("\\.000000$","",df[,c],perl=T)
  rm(c)
  assign(file,df)
}

for(file in files_exp) {
  df <- get(file)
  for (c in c("V1","V3","V5","V9","V10","V12","V15","V16","V18","V19","V21","V22","V25")) df[,c] <- gsub("\\.000000$","",df[,c],perl=T)
  rm(c)
  assign(file,df)
}

#Virer les lignes sans id sur 3 colonnes
for(file in c(files_stk,files_exp)) {
  df <- get(file)
  df <- df[which(!is.na(df$V1) & !is.na(df$V2) & !is.na(df$V3)),]
  df <- df[which(df$V1 !="NA" & df$V2 !="NA" & df$V3 !="NA"),]
  df <- df[which(df$V1 !="" & df$V2 !="" & df$V3 !=""),]
  df <- df[which(df$V1 !="0" & df$V2 !="0" & df$V3 !="0"),]
  assign(file,df)
}


rm(c,df,file)