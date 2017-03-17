d3Sankey(Links = EngLinks, Nodes = EngNodes, Source = "source",
         Target = "target", Value = "value", NodeID = "name",
         fontsize = 12, nodeWidth = 30, file = "~/TestSankey.html")


i <- 2

for ( c in names(res)) res[,c] <- as.character(res[,c])
for ( c in names(res)) res[,c] <- as.character(res[,c])
df$NBUNT <- gsub("\\,",".",df$NBUNT)

assoc <- data.frame(NULL)


for (i in 1:length(res[,1])) {
  unt_sum <- sum(as.numeric(df[df$LBPAY == res[i,1] & df$LBLAB == res[i,2] , "NBUNT"]),na.rm =T)
  assoc <- rbind(assoc,c(res[i,1],res[i,2],unt_sum))
  names(assoc) <- c("source","target","NB")
  for ( c in names(assoc)) assoc[,c] <- as.character(assoc[,c])
}


d3Sankey(Links = EngLinks, Nodes = EngNodes, Source = "source",
         Target = "target", Value = "value", NodeID = "name",
         fontsize = 12, nodeWidth = 30, file = "~/Desktop/TestSankey.html")
x <- readLines(con = "C:/Users/ottavig/Documents/TestSankey.html")
x <- x[-28]
writeLines(text = x ,con = "C:/Users/ottavig/Documents/TestSankey.html")

# Data source: http://goo.gl/vcKo6y
UKvisits <- data.frame(origin=c(
  "France", "Germany", "USA",
  "Irish Republic", "Netherlands",
  "Spain", "Italy", "Poland",
  "Belgium", "Australia", 
  "Other countries", rep("UK", 5)),
  visit=c(
    rep("UK", 11), "Scotland",
    "Wales", "Northern Ireland", 
    "England", "London"),
  weights=c(
    c(12,10,9,8,6,6,5,4,4,3,33)/100*31.8, 
    c(2.2,0.9,0.4,12.8,15.5)))
## Uncomment the next 3 lines to install the developer version of googleVis
# install.packages(c("devtools","RJSONIO", "knitr", "shiny", "httpuv"))
library(devtools)
# install_github("mages/googleVis")
require(googleVis)
plot(
  gvisSankey(UKvisits, from="origin", 
             to="visit", weight="weight",
             options=list(
               height=250,
               sankey="{link:{color:{fill:'purple'}}}"
             ))
)