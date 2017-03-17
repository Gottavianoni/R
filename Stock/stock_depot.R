#Stockdepot
library(readxl)
chemin <- "C:/Users/ottavig/Desktop/stock_ods/"

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
  
  if(typeof(df) != "list") {
    df <- as.character(df)
    df <- tolower(df)
    if(regexpr("need one name",df,fixed=T) != -1) {
     i <- 15
     while (i < 30){
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

#rm(df,fil_xls,file,fil,nb_col,xls,path)

#lecture des csv/txt
txt <- fichiers[regexpr("(csv+$|txt+$)",extension,perl =T) != -1]
fil_txt <- filiale[regexpr("(csv+$|txt+$)",extension,perl= T) != -1]
typ_txt <- type[regexpr("(csv+$|txt+$)",extension,perl= T) != -1]

if(exists("er_xls")) txt <- c(txt,er_xls)
if(exists("er_filiale")) fil_txt <- c(fil_txt,er_filiale)
if(exists("er_type")) typ_txt <- c(typ_txt,er_type)

for (file in txt) {
  path <- paste0(chemin,file)
  fil <- paste0("F",fil_txt[which(file == txt)],typ_txt[which(file == txt)])
  df1 <- try(read.csv(path,colClasses = "character",header = F,skipNul = T))
  df2 <- try(read.csv2(path,colClasses = "character",header = F,skipNul = T))
  df3 <- try(read.delim(path, header = F, sep = "\t",colClasses = "character", fill = F,skipNul = T))
  df4 <- try(read.delim2(path, header = F, sep = "\t",colClasses = "character", fill = F,skipNul = T))
  lt <- c(length(df1), length(df2),length(df3), length(df4))
  lt <- which(lt == max(lt))[1]
  df <- get(paste0("df",lt))

  
  if(regexpr("F[0-9][0-9][0-9]",fil,perl =T) == -1 ){
    assign(paste0("F",y),df)
    y <- y+1
    rm(list = fil)
  }
  else{
    assign(fil,df)
  }
}

rm(df1,df2,df3,df4,file,file,fil_txt,lt,path,txt,df,fil,typ_txt,err_filiale,er_type,er_xls,extension,sh,typ_xls,x,type,ls_fil,er_filiale,fichiers,filiale)

file_check <- ls()
file_check <- file_check[regexpr("F[0-9][0-9][0-9][a-z][a-z][a-z]",file_check,perl= T) == -1]
for (fil in file_check) {
  df <- get(fil)
  if (length(names(get(fil))) >= 21 & length(names(get(fil))) < 30 ) assign (paste0(substr(fil,1,4),"exp"),df)
  if (length(names(get(fil))) > 12 & length(names(get(fil))) < 21 )  assign (paste0(substr(fil,1,4),"stk"),df) 
  rm(list = fil)
}
rm(df,fil,file_check)
