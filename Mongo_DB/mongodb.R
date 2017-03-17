#MONGODB
library(df2json)
library(rmongodb)

#EXECUTER UN COMMANDE avec la mention wait=F

mongo <- mongo.create()
if(mongo.is.connected(mongo) == TRUE) {
mongo.get.databases(mongo)
}

files <- list.files("A:/Extraction_Full/avene/")
df <- NULL
for (f in files) try(df <- rbind(df,read.csv2(paste0("A:/Extraction_Full/avene/",f),colClasses = "character")[,1:5]))

js_df <- df2json(df)

#NS DECOMPOSITION -> database.collection SI PAS EXISTANT CA LA CREE
#IL FAUT REMPLIR LA COLLECTION POUR  QU'ELLE APPARAISSENT

ns <- "prix_internet.ducray"
json <- js_df
bson <- mongo.bson.from.JSON(json)

mongo.insert(mongo, ns, ducray)
mongo

library(rmongodb)
system2("C:/mongodb/bin/mongod.exe",wait = F)
mongo <- mongo.create()
mongo.get.databases(mongo)
mongo.get.database.collections(mongo , db = "prix_internet")
mongo.command(mongo = mongo, db = "IMS", command = list(listCollections=1))

duc <- "test.fact"
duc <- "IMS.fact"

for (i in 1:500) mongo.insert(mongo, duc, mongo.bson.from.df(ff[i,])[[1]])
ducray <- mongo.bson.from.df(list(ff[i,]))
cityone <- mongo.find.one(mongo, duc)
print(cityone)
mongo.bson.to.list(cityone)
res <- mongo.distinct(mongo, duc, "10")
mongo.insert(mongo, duc, ducray)
mongo.count(mongo,duc)
mongo.drop.database(mongo,"IMS")

library(ff)
mongo.find.all(mongo,duc)
mongo.find.all(mongo,duc, '{ "Manufacturer" : "AVENE" }')

cursor <- mongo.find(mongo,duc, '{ "Manufacturer" : "AVENE" }')
df <- NULL

while(mongo.cursor.next(cursor)) {
     value <- mongo.cursor.value(cursor)
     liste <- mongo.bson.to.list(value)
     name <- names(liste)
     df <- data.frame(rbind(df,liste))
}






