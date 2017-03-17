#Analyse Semantique
#avis <- read.csv2("S:/Documents/Archive/2015_04_08_Avene_etude/triacneal/source_triacneal.csv")
#avis <- read.csv2("C:/Documents and Settings/ottavig/Bureau/Reunion_Olivier/Elancyl_Slim_Concurrence/Donnees/cellu_soma_comparaison.csv")

mots_vides <- read.table("C:/Users/ottavig/Documents/Script_R/Mots_Vides/mots_vides.csv", quote="\"", stringsAsFactors=FALSE)
mots_vides <- as.list(mots_vides[,1])

mots_usage <- c("produit","levres","bon","bonne",
                'bien',"creme","acne","peau","Peau",
                "boutons","bouton","utilise","cheveux",
                "mois","utilisation","resultats","resultats","vraiment","aime",
                "utiliser","trouve","meme","laisse")

mots_vides <- c(mots_vides,mots_usage)

for (col in c("pseudo_avis","an","ap","com")) {
  avis[,col] <- gsub("é","e",avis[,col],fixed = T)
  avis[,col] <- gsub("è","e",avis[,col],fixed = T)
  avis[,col] <- gsub("â","a",avis[,col],fixed = T) 
  avis[,col] <- gsub("é","e",avis[,col],fixed = T)
  avis[,col] <- gsub("è","e",avis[,col],fixed = T)
  avis[,col] <- gsub("ê","i",avis[,col],fixed = T) 
  avis[,col] <- gsub("â","a",avis[,col],fixed = T)
  avis[,col] <- gsub("î","i",avis[,col],fixed = T)
  avis[,col] <- gsub("ç","c",avis[,col],fixed = T)  
  avis[,col] <- gsub("û","u",avis[,col],fixed = T)
  avis[,col] <- gsub("�","a",avis[,col],fixed = T)
  avis[,col] <- gsub("(\n)", " ",avis[,col],perl = T)
  avis[,col] <- gsub("(\r)", " ",avis[,col],perl = T)
  avis[,col] <- gsub('(\")', " ",avis[,col],perl = T)
  avis[,col] <- gsub('(")', " ",avis[,col],perl = T)
  avis[,col] <- gsub("a*", "",avis[,col],fixed = T)
  avis[,col] <- gsub("/", " ",avis[,col],fixed = T)
  avis[,col] <- gsub("!", " ",avis[,col],fixed = T)
  avis[,col] <- gsub("'", " ",avis[,col],fixed = T)
  avis[,col] <- gsub("*", " ",avis[,col],fixed = T)
  avis[,col] <- gsub("-", " ",avis[,col],fixed = T)
  avis[,col] <- gsub(".", " ",avis[,col],fixed = T)
  avis[,col] <- gsub(",", " ",avis[,col],fixed = T)
  avis[,col] <- gsub("?", " ",avis[,col],fixed = T)
  avis[,col] <- gsub(")", " ",avis[,col],fixed = T)
  avis[,col] <- gsub("(", " ",avis[,col],fixed = T)  
  avis[,col] <- gsub("#", " ",avis[,col],fixed = T)
  avis[,col] <- gsub("&", " ",avis[,col],fixed = T)
  avis[,col] <- gsub("[", " ",avis[,col],fixed = T)    
  avis[,col] <- gsub("]", " ",avis[,col],fixed = T)
  avis[,col] <- gsub(":", " ",avis[,col],fixed = T)
  avis[,col] <- gsub("~", " ",avis[,col],fixed = T)
  avis[,col] <- gsub("=", " ",avis[,col],fixed = T)
  avis[,col] <- gsub("+", " ",avis[,col],fixed = T)
  avis[,col] <- gsub("^", " ",avis[,col],fixed = T)
  avis[,col] <- gsub("_", " ",avis[,col],fixed = T)
  avis[,col] <- gsub(">", " ",avis[,col],fixed = T)
  avis[,col] <- gsub("#NOM", "",avis[,col],fixed = T)
  avis[,col] <- gsub("./","",avis[,col],fixed = T)
  avis[,col] <- tolower(avis[,col])
}

rm(col)

for (col in names(avis)) avis[,col] <- as.character(avis[,col])

col <- "ap"

liste <- unlist(strsplit(avis[,col],split = " "))
res <- ""
for(mot in liste) {
res <- c(res,mot)
}

rm(mot,liste)

res <- res[which(res != "")]
res <- res[which(res != "-")]
res <- res[which(res != "*")]
res <- tolower(res)

taille <- length(which(!is.na(match(mots_vides,res)))) 
while (taille != 0) {
table <- match(mots_vides,res)
table <- table[!is.na(table)]
res <- res[-table]
taille <- length(which(!is.na(match(mots_vides,res))))
}

res <- res[which(nchar(res) > 2)]
rm(table,taille)

resul <- data.frame(as.character(res))
res_analyse <- data.frame(table(resul))
res_analyse$ratio <- res_analyse$Freq / sum(res_analyse$Freq,na.rm = T)
res_analyse <- res_analyse[order(res_analyse$Freq,decreasing = T),]
top_analyse <- res_analyse[res_analyse$ratio > 0.005,]
library("wordcloud")
wordcloud(res_analyse[,1],res_analyse[,2],random.color = F,random.order = F,max.word = 1000,color = brewer.pal(n = 8,name = "Set2"))
rm(res,resul)