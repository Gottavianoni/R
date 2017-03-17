#ANALYSE AQD PT

PT_2015 <- read.csv2("C:/Users/ottavig/Desktop/PT2016.csv", stringsAsFactors=FALSE, colClasses = "character")

PT_2015 <- PT_2015[,c(8,10:13,16,18:27)] 
names(PT_2015) <- c("COART","TYART","STYART","MDL"
                    ,"CDV","PGP","CRISTD","CRI2016","CRIBIS"
                    ,"PPHT2015","PPHT2016","COPAY","PTEU"
                    ,"PTDE","OTCO","FTCO")

names(PT_2015) <- c("COPAY","LBPAY","PGP","GTIN"
                    ,"COART","MDL","CDV","DTMOD","PPHT"
                    ,"DTCRI","CRISTD","CRI2016","DTPT"
                    ,"PTEU","PTDE","OTCO","FTCO","ARC")

PT_2015 <- PT_2015[PT_2015$PGP != "-",]
PT_2015 <- PT_2015[PT_2015$PGP != "",]
PT_2015 <- PT_2015[PT_2015$PPHT != "",]
PT_2015 <- PT_2015[PT_2015$CRI != PT_2015$PTEU & PT_2015$CRISTD != PT_2015$PTEU  ,]


PT_2015 <- PT_2015[PT_2015$MDL %in% c("VTE","HOP"),]
PT_2015 <- PT_2015[PT_2015$PGP != "-",]
PT_2015 <- PT_2015[PT_2015$PGP != "",]
PT_2015 <- PT_2015[!is.na(PT_2015$PGP),]
PT_2015$PPHT <- ifelse(PT_2015$PPHT2016 == "",PT_2015$PPHT2015,PT_2015$PPHT2016)
PT_2015$CRI <- ifelse(PT_2015$CRI2016 != "",PT_2015$CRI2016,PT_2015$CRISTD)
PT_2015 <- PT_2015[PT_2015$CRI != PT_2015$PTEU ,]
PT_2015 <- PT_2015[PT_2015$CRI != PT_2015$PTDE ,]

PT_2015$TCO <- ifelse(PT_2015$OTCO != "" & PT_2015$FTCO == "", "O" ,"F")
#POUR OUVERT CO UNIQUEMENT
PT_2015 <- PT_2015[PT_2015$TCO == "O",]

names(PT_2015) <- c("CLE","COPAY","PGP","COART","PPHT","PTEU","PTDE","CDV" )
names(PT_2015) <- c("CLE","COPAY","PGP","COART","PPHT","PTEU","PTDE","CDV" )

PT_2015$id_3 <- paste0(PT_2015$COPAY,PT_2015$PGP,PT_2015$PPHT)
uni <- unique(PT_2015$id_3)
PT_2015$test_id3 <- "AF"
PT_2015$test_id4 <- "AF"
PT_2015 <- PT_2015[PT_2015$PGP != "-",]
PT_2015 <- PT_2015[!is.na(PT_2015$PGP),]
PT_2015 <- PT_2015[PT_2015$PGP != "",]
PT_2015_excl <- PT_2015[PT_2015$CRI == PT_2015$PTEU & PT_2015$CRISTD != PT_2015$PTEU,]
PT_2015 <- PT_2015[PT_2015$CRI != PT_2015$PTEU & PT_2015$CRISTD != PT_2015$PTEU  ,]

PT_2015_excl <- PT_2015[PT_2015$CRI == PT_2015$PTEU,]
PT_2015 <- PT_2015[PT_2015$CRI != PT_2015$PTEU,]


for (i in 1:length(uni)) {
  tst <- which(PT_2015$id_3 == uni[i])
  if (length(tst) > 1) {
    if(length(which(PT_2015[tst[1],"PTDE"] == PT_2015[tst,"PTDE"])) == length(tst)) {
      PT_2015[tst,"test_id3"] <- "OK" 
    } 
    
      else  {
        px <- which(min(PT_2015[tst,"CDV"]) == PT_2015[tst,"CDV"])
        if (length(px) > 1) {
          
          x <- sort(table(PT_2015[tst[px],"PTDE"]),decreasing = T)
          if (length(x) > 1) {
            if(x[1] == x[2]){
              x2 <- max(as.numeric(gsub(",","\\.",names(x))))
              x2 <- gsub("\\.",",",as.character(x2))
              PT_2015[tst,"test_id3"] <- paste0("PviaCDV_approx_sup : ",x2)
            } 
            else PT_2015[tst,"test_id3"] <- paste0("PviaCDV_approx_max : ",names(x[1]))
          }
            else {
          px <- px[1]<
          print(PT_2015[px[1],"PGP"])
          PT_2015[tst,"test_id3"] <- paste0("PviaCDV_approx_uni : ",PT_2015[tst[px[1]],"PTDE"])
          }
        }
        else PT_2015[tst,"test_id3"] <- paste0("PviaCDV : ",PT_2015[tst[px],"PTDE"])  
        }
  }
  else PT_2015[tst,"test_id3"] <- "OK"
}

ind <- which(regexpr("[0-9]*\\,[0-9]*",PT_2015$test_id3) > 0)
ind2 <- attr(regexpr("[0-9]*\\,[0-9]*",PT_2015$test_id3),"match.length")
ind2 <- ind2[ind]
ind3 <- regexpr("[0-9]+\\,*[0-9]+",PT_2015$test_id3)
ind3 <- ind3[ind]
PT_2015$val_sub <- "OK" 
PT_2015[ind,"val_sub"] <- substr(PT_2015[ind,"test_id3"],ind3,ind3 + ind2)

test <- which(regexpr("\\: [0-9]*$",PT_2015$test_id3) != -1)
test1 <- regexpr("\\: [0-9]*$",PT_2015$test_id3)
test1 <- test1[test]
test3 <- attr(regexpr("\\: [0-9]*$",PT_2015$test_id3),"match.length")
test3 <- test3[test]
PT_2015[test,"val_sub"] <- substr(PT_2015[test,"test_id3"],test1+2,test1+test3)

PT_2015$flag_modif_de <- ifelse(PT_2015$val_sub == PT_2015$PTDE |PT_2015$val_sub == "OK" ,"OK","KO")

write.csv2(PT_2015,"C:/users/ottavig/Desktop/PT_2016_check.csv",row.names = F)


#PF FINAL

PT_F <- read.csv2("C:/users/ottavig/Desktop/PT_f.csv",colClasses = "character")
PT_F <- PT_F[PT_F[,1] != "",]
headers <- names(PT_F)[1:6]
x <- PT_F
final <- NULL
for(i in 1:62){
  temp <- x[,c(1:6)]
  names(temp) <- headers
  final <- rbind(final,temp)
  x <- x[,-c(1:6)]
}
  


for (i in 1:length(uni)) {
  tst <- which(PT_2015$id_3 == uni[i])
  if (length(tst) > 1) {
    if(length(which(PT_2015[tst[1],"PTEU"] == PT_2015[tst,"PTEU"])) == length(tst)) {
      PT_2015[tst,"test_id4"] <- "OK" 
    } 
    
    else  {
      px <- which(min(PT_2015[tst,"CDV"]) == PT_2015[tst,"CDV"])
      if (length(px) > 1) {
        
        x <- sort(table(PT_2015[tst[px],"PTEU"]),decreasing = T)
        if (length(x) > 1) {
          if(x[1] == x[2]){
            x2 <- max(as.numeric(gsub(",","\\.",names(x))))
            x2 <- gsub("\\.",",",as.character(x2))           
            PT_2015[tst,"test_id4"] <- paste0("PviaCDV_approx_sup : ",x2)
          } 
          else PT_2015[tst,"test_id4"] <- paste0("PviaCDV_approx_max : ",names(x[1]))
        }
        else {
          px <- px[1]
          print(PT_2015[px[1],"PGP"])
          PT_2015[tst,"test_id4"] <- paste0("PviaCDV_approx_uni : ",PT_2015[tst[px[1]],"PTEU"])
        }
      }
      else PT_2015[tst,"test_id4"] <- paste0("PviaCDV : ",PT_2015[tst[px],"PTEU"])  
    }
  }
  else PT_2015[tst,"test_id4"] <- "OK"
}



ind <- which(regexpr("[0-9]*\\,[0-9]*",PT_2015$test_id4) > 0)
ind2 <- attr(regexpr("[0-9]*\\,[0-9]*",PT_2015$test_id4),"match.length")
ind2 <- ind2[ind]
ind3 <- regexpr("[0-9]+\\,*[0-9]+",PT_2015$test_id4)
ind3 <- ind3[ind]
PT_2015$val_sub_eu <- "OK" 
PT_2015[ind,"val_sub_eu"] <- substr(PT_2015[ind,"test_id4"],ind3,ind3 + ind2)

test <- which(regexpr("\\: [0-9]$",PT_2015$test_id4) != -1)
test1 <- regexpr("\\: [0-9]$",PT_2015$test_id4)
test1 <- test1[test]
PT_2015[test,"val_sub_eu"] <- substr(PT_2015[test,"test_id4"],test1+2,test1+3)

PT_2015$flag_modif_eu <- ifelse(PT_2015$val_sub_eu == PT_2015$PTEU | PT_2015$val_sub_eu == "OK" ,"OK","KO")

write.csv2(PT_2015,"C:/users/ottavig/Desktop/PT_2016_check.csv",row.names = F)

PT_2015$vaar <- as.numeric(gsub("\\,","\\.",PT_2015[,"PTEU"]))
res_sd <- data.frame(tapply(X = PT_2015$vaar,INDEX = as.factor(PT_2015$id_3),sd))
x <- data.frame(table(PT_2015$id_3))
res_sd <- cbind(res_sd,x)


---------------------------------------------
PT_2015$test_id3 <- "AF"
PT_2015$test_id4 <- "AF"
  
    for (i in 1:length(uni)) {
    tst <- which(PT_2015$id_3 == uni[i])
    if(length(tst) > 4) print (PT_2015[tst,"id_3"])
  }  

  for (i in 1:length(uni)) {
    tst <- which(PT_2015$id_3 == uni[i])
    dtmod <- as.numeric(paste0(substr(PT_2015[tst,"DTPT"],7,10),substr(PT_2015[tst,"DTPT"],4,5),substr(PT_2015[tst,"DTPT"],1,2)))
    if (length(tst) > 1) {
      if(length(which(PT_2015[tst[1],"PTDE"] == PT_2015[tst,"PTDE"])) == length(tst)) {
        PT_2015[tst,"test_id4"] <- "OK" 
      } else {
        px <- which(PT_2015[tst,"TCO"] == "O" & dtmod >= 20160101 & dtmod <= 20160110)
        x <- sort(table(PT_2015[tst[px],"PTEU"]),decreasing = T)
        if(length(px) == 1 ) {
          PT_2015[tst,"test_id4"] <- paste0("PviaOTCO : ",PT_2015[tst[px],"PTEU"])  
        } else { if (length(x) == 1 ) {
          px <- px[1]
          PT_2015[tst,"test_id4"] <- paste0("PviaOTCO_uni : ",PT_2015[tst[px],"PTEU"])  
        } else { if (length(x) > 1 & x[1] != x[2] ) {
          px <- names(x[1])
          PT_2015[tst,"test_id4"] <- paste0("PviaOTCO_max : ",px) 
        } else { if (length(x) > 1 & x[1] == x[2]) {
          dtco <- as.numeric(paste0(substr(PT_2015[tst[px],"OTCO"],7,10),substr(PT_2015[tst[px],"OTCO"],4,5),substr(PT_2015[tst[px],"OTCO"],1,2)))
          px <- px[which(max(dtco) == dtco)][1]
          PT_2015[tst,"test_id4"] <- paste0("PviaOTCO_sup : ",PT_2015[tst[px],"PTEU"])
        }
          
          }
            } 
      }
      }
    }
    else PT_2015[tst,"test_id4"] <- "OK"
}