#Fichiers stk et tra manquant
filiale <-   c("006","010","014","024","025","026","027","029","030","034","049","053","064","066","067","070","071","073","079","081","301","303","305","306","308")
df <- data.frame(rbind(wrld_stk = rep("",length(filiale)),wrld_tra = rep("",length(filiale))))
names(df) <- filiale
for ( c in names(df)) df[,c] <- as.character(df[,c])

for (i in 1:length(filiale)) {
  fil <- filiale[i]
  repo <- "C:/Users/ottavig/Desktop/test/"
  repo <- paste0(repo,fil)
  if(dir.exists(repo))  {
  stk <- paste0(repo,"/wrld_stkn_",fil,".txt")
  if(file.exists(stk)) df[1,i] <- "X"
  tra <- paste0(repo,"/wrld_tra_",fil,".csv")
  if(file.exists(tra)) df[2,i] <- "X"
  }
}

df[,length(filiale)+1] <- ""
names(df) <- c(" ",filiale)

write.table(x = df,file = paste0("C:/Users/ottavig/Desktop/test/Bilan/BILAN_STOCK_",Sys.Date(),".csv") , sep =";",row.names = T,col.names = T)
rm(list = ls())
