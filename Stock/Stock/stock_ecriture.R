#Ecriture des fichiers dans des repertoire au format dossier N0 filliale


for (file in files_stk) {
  df <- get(file)
  repo <- "O:/01-MAINTENANCE SI/ODS STOCK/stock_ods/"
  filiale <- substr(file,2,4)
  repo <- paste0(repo,filiale)
  if(!dir.exists(repo)) dir.create(repo)
  fich <- paste0(repo,"/wrld_stkn_",filiale,".txt")
  write.table(x = df,file = fich, sep =";",quote = F,row.names = F,col.names= F)
}

for (file in files_exp) {
  df <- get(file)
  repo <- "O:/01-MAINTENANCE SI/ODS STOCK/stock_ods/"
  filiale <- substr(file,2,4)
  repo <- paste0(repo,filiale)
  if(!dir.exists(repo)) dir.create(repo)
  fich <- paste0(repo,"/wrld_tra_",filiale,".csv")
  write.table(x = df,file = fich, sep =";",quote = F,row.names = F,col.names = TRUE)
}

rm(list = ls())

print("#######################################################")
print("#######################################################")
print("#############      EXECUTION TERMINE      #############")
print("#######################################################")
print("#######################################################")

rm(list = ls())



 
