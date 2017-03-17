library(readxl)
#if(regexpr("(csv$|xlsx$|xls$|txt$)",fichier,perl = T) != -1 & filiale %in% list_fil) {
  flag_enc <- "KO"
  file <- fichier
  if (extension %in% c("xls","xlsx") ) {
    
    path <- paste0(path,fichier)
    
    #Compteur de feuille
    sh <- 1
    
    #MAUVAISE FEUILLLE (VIDE OU INAPROPRIEE)
    df <- try(read_excel(path,sheet = sh,col_names= F))
    #Contr?le des fichiers mauvaise feuille
    if(typeof(df) == "character") x <- c(x,file)
    if(typeof(df) == "character" | length(df) < 10) {df <- try(read_excel(path,sheet = 2,col_names= F));sh <- 2;}
    if(typeof(df) == "character" | length(df) < 10) {df <- try(read_excel(path,sheet = 3,col_names= F));sh <- 3;}
    if(typeof(df) == "character" | length(df) < 10) {df <- try(read_excel(path,sheet = 4,col_names= F));sh <- 4;}
    
    #ERREUR CAR TXT/csv renommer en .xls
      if(typeof(df) == "character" && regexpr("Failed to open",df,fixed=T) != -1) flag_enc <- "OK"
    
    if(min(as.integer(substr(names(df),2,5))) == 0 ) nb_col <- max(as.integer(substr(names(df),2,5))) +1 else  nb_col <- max(as.integer(substr(names(df),2,5)))
    
    df <- try(data.frame(read_excel(path, sheet = sh , col_names = paste0("V",1:nb_col),col_types = rep("text",nb_col))))
    if(typeof(df) == "list") assign(name,df)
  }
  rm(df1,df2,df3,df4)
  
  if (extension %in% c("txt","csv") | flag_enc == "OK") {
    path <- paste0(path,fichier)
    df1 <- try(read.csv(path,colClasses = "character",header = F,skipNul = T))
    df2 <- try(read.csv2(path,colClasses = "character",header = F,skipNul = T))
    df3 <- try(read.delim(path, header = F, sep = "\t",colClasses = "character", fill = F,skipNul = T))
    df4 <- try(read.delim2(path, header = F, sep = "\t",colClasses = "character", fill = F,skipNul = T))
    lt <- c(length(df1), length(df2),length(df3), length(df4))
    lt <- which(lt == max(lt))[1]
    df <- get(paste0("df",lt))
    df[,1] <- gsub("??","",df[,1],fixed=T)
    try(assign(name,df))
  }
  rm(lt)
#}

