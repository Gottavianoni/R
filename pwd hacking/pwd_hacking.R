library("RODBC")

RPG <- odbcConnect(dsn = "*****",uid = "*****",pwd = "*****")

query <- ("SELECT USERNAME FROM dba_users")

users <-  as.character(sqlQuery(DWH,query)[,1])

dsn <- "*****"

for ( utl in users){
assign(paste0("USER_",utl),odbcConnect(dsn = dsn ,uid = utl,pwd = utl))
if(get(paste0("USER_",utl)) == -1) rm(list = paste0("USER_",utl))
Sys.sleep(0.5)
}

pwd <- users

ind_1 <- which(regexpr("UTL",pwd)!=-1)
pwd[ind_1] <- paste0(substr(pwd[ind_1],4,100),substr(pwd[ind_1],1,3))

ind_2 <- which(regexpr("DBO",pwd)!=-1)
pwd[ind_2] <- paste0(substr(pwd[ind_2],4,100),substr(pwd[ind_2],1,3))

for ( i in 1:length(users)){
  assign(paste0("USER_",users[i]),odbcConnect(dsn = dsn,uid = users[i],pwd = pwd[i]))
  if(get(paste0("USER_",users[i])) == -1) rm(list = paste0("USER_",users[i]))
  Sys.sleep(0.5)
}

assign(paste0("USER_",users[i]),odbcConnect(dsn = "*****",uid = "*****",pwd = "*****"))


MDM <- odbcConnect(dsn = "*****",uid = "*****",pwd = "*****")
DWH <- odbcConnect(dsn = "*****",uid = "*****",pwd = "*****")


query <- "update MDMDBO.T_MD_PF_CODE_LOCAL SET T_MD_PF_CODE_LOCAL.CDESIGNATION_CODE_LOCAL = 'AD PRIMALBA GEL LAV 500ML 7LA'
WHERE T_MD_PF_CODE_LOCAL.CPRODUCTKEYID='3657300'"

query <- "SELECT * FROM MDMDBO.T_MD_PF_CODE_LOCAL
WHERE T_MD_PF_CODE_LOCAL.CPRODUCTKEYID='3657300'"

df <- sqlQuery(MDM,query)
