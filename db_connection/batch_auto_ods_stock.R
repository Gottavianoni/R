#BATCH AUTOMATIQUE EN BOUCLE

path <- "C:/Users/ottavig/Desktop/stock_ods/robot_tra/"
code_pays <- c("989","960","957","999","998","997","995","996","991","992","990","985","971","978","970","974","982","980","969","965","981","956","955","946","953","964","845")
filiale <-   c("006","010","014","024","025","026","027","029","030","034","049","053","061","064","066","067","070","071","073","079","081","301","303","305","306","308","312")

library(RODBC)
rov <- odbcConnect(dsn = "*****",uid = "*****",pwd = "*****")

query <- ("SELECT S.COFLL,S.AA, S.MM, SUM(QTSTKREE) as QTE, SUM(VLACH) as STOCK_VAL, SUM(VLFRSAPH) as FRAIS_APPROCHE, sum(VLAJTLCA) as VALEUR_AJ, sum(VLPVSDPCLCA) as PROVISIONS 
FROM ADMODS.D_STKMRGINT_ODS S
WHERE S.TYSTK = 'C'
GROUP BY S.COFLL, S.AA,S.MM
ORDER BY CAST(S.COFLL as INTEGER) , CAST(S.MM as INTEGER)")
df <- sqlQuery(rov,query)
print("REQUETE EN COURS D'EXECUTION")

df$idm <- as.numeric(paste0(df$AA,df$MM))
ind <- max(as.numeric(paste0(df$AA,df$MM)))
df <- df[df$idm == ind,]

if(length(df[,1]) == length(code_pays))
