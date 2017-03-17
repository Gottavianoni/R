fut_stats <- read.csv2("C:/Users/ottavig/Documents/Script_R/Stats FUT/graph/fut_stats.csv")
fut_stats <- as.character(fut_stats[,1])
fut_stats <- fut_stats[regexpr("Overa",fut_stats) == -1]

names <- fut_stats[which(regexpr(" gp",fut_stats) != -1) -1] 
ast <- fut_stats[which(regexpr(" ast",fut_stats) != -1)]
gls <- fut_stats[which(regexpr("gls",fut_stats) != -1)]
gp <- fut_stats[which(regexpr("gp",fut_stats) != -1)]
pass <- fut_stats[which(regexpr("pass",fut_stats) != -1)]
pra <- fut_stats[which(regexpr("p\\%",fut_stats) != -1)]
tkl <- fut_stats[which(regexpr("tkl$",fut_stats) != -1)]
tklra <- fut_stats[which(regexpr("tkl\\%",fut_stats) != -1)]
wra<- fut_stats[which(regexpr("w\\%",fut_stats) != -1)]


res <- data.frame(cbind(names,gls,gp,ast,pass,pra,tkl,tklra,wra))
for(c in names(res)[-1]) res[,c] <- as.numeric(substr(res[,c],regexpr("[0-9]{1,}",res[,c]),regexpr("[0-9]{1,}",res[,c]) + attr(regexpr("[0-9]{1,}",res[,c]),"match.length")-1))

res$glspm <- round(res$gls  * 100/ res$gp)
res$astpm <- round(res$ast * 100/ res$gp)
res$tklpm <- round(res$tkl / res$gp,1)
res$gppm <-  round(res$gp * 100/ 287)
res$passpm <-  round(res$pass / res$gp)

