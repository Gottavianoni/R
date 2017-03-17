#Stockdepot
library(readxl)
chemin <- "C:/Users/ottavig/Desktop/test/"

#lecture d'une repertoire
fichiers <- list.files(chemin)
fichiers <- fichiers[regexpr("(csv$|xlsx$|xls$|txt$)",fichiers,perl = T)!= -1]

extension <- substr(fichiers,regexpr("(\\.x|\\.c|\\.t)",fichiers,perl=T)+1,nchar(fichiers))
filiale <- substr(fichiers,regexpr("[0-9][0-9][0-9]",fichiers),regexpr("[0-9][0-9][0-9]",fichiers)+2)
filiale <- ifelse(nchar(filiale) != 3,paste0("0",substr(fichiers,regexpr("_[0-9][0-9]\\.",fichiers)+1,regexpr("_[0-9][0-9]\\.",fichiers)+2)),filiale)
filiale <- ifelse(nchar(filiale) != 3,paste0("0",substr(fichiers,regexpr("_[0-9]\\.",fichiers)+1,regexpr("_[0-9]\\.",fichiers)+2)),filiale)
type <- substr(tolower(fichiers),regexpr("(st+|exp+|tra+|vte+|art+)",tolower(fichiers),perl =T),regexpr("(st+|exp+|tra+|vte+|art+)",tolower(fichiers),perl=T)+2)
type <- ifelse(type == "sto","stk",type)
type <- ifelse(type == "tra","exp",type)


#Charger des Fichiers Excel xls et xlsx
xls <- fichiers[regexpr("(xls+$|xlsx+$)",extension,perl =T) != -1]
fil_xls <- filiale[regexpr("(xls+$|xlsx+$)",extension,perl= T) != -1]
typ_xls <- type[regexpr("(xls+$|xlsx+$)",extension,perl= T) != -1]

x <- ""
y <- 1
for (file in xls) {
  path <- paste0(chemin,file)
  fil <- paste0("F",fil_xls[which(file == xls)],typ_xls[which(file == xls)])
  print(file)
  #Compteur de feuille
  sh <- 1

  #MAUVAISE FEUILLLE (VIDE OU INAPROPRIEE)
  df <- try(read_excel(path,sheet = sh,col_names= F))
  #Controle des fichiers mauvaise feuille
  if(typeof(df) == "character") x <- c(x,file)
  if(typeof(df) == "character" | length(df) < 10) {df <- try(read_excel(path,sheet = 2,col_names= F));sh <- 2;}
  if(typeof(df) == "character" | length(df) < 10) {df <- try(read_excel(path,sheet = 3,col_names= F));sh <- 3;}
  if(typeof(df) == "character" | length(df) < 10) {df <- try(read_excel(path,sheet = 4,col_names= F));sh <- 4;}
  
  #Fichier pas Excel
  if(typeof(df) == "character" && regexpr("Failed to open",df,fixed=T) != -1) {
  if(!exists("er_xls")) {er_xls <- file;er_filiale <- fil_xls[which(file == xls)];er_type <- typ_xls[which(file == xls)];}
    else{er_xls <- c(er_xls,file);er_filiale <- c(er_filiale,fil_xls[which(file == xls)]);er_type <- c(er_type,typ_xls[which(file == xls)]);}
  }

  if(min(as.integer(substr(names(df),2,5))) == 0 ) nb_col <- max(as.integer(substr(names(df),2,5))) +1 else  nb_col <- max(as.integer(substr(names(df),2,5)))
  
  df <- try(data.frame(read_excel(path, sheet = sh , col_names = paste0("V",1:nb_col),col_types = rep("text",nb_col))))
  print(typeof(df))
  if(typeof(df) != "list") {
    df <- as.character(df)
    df <- tolower(df)
    print(df)
    if(regexpr("need one name",df,fixed=T) != -1) {
     i <- 15
     while (i < 30){
      print(file,i)
     df <- try(data.frame(read_excel(path, sheet = sh , col_names = paste0("V",1:i),col_types = rep("text",i))))
     if (typeof(df) == "list") i = 30
     i <- i + 1
     }
   }
  }
  
  if(typeof(df) == "list") {
      if(regexpr("F[0-9][0-9][0-9]",fil,perl =T) == -1 ){
        assign(paste0("F",y),df)
        y <- y+1
        rm(list = fil)
        }
    else{
      assign(fil,df)
        }
    }
}
