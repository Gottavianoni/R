#necessary file for Windows
#download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")

library(twitteR)

#to get your consumerKey and consumerSecret see the twitteR documentation for instructions
consumer_key <- "*****"
consumer_secret <- "*****"
access_token <- "*****"
access_secret <- "*****"
setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_secret)

r_stats <- searchTwitter("galenic", lang = "fr", n = 2000)
x <- twListToDF(twList = r_stats)
avis <- paste(x[,1],collapse = " ")

install.packages("Rfacebook")
library(Rfacebook)

token <- "*****"
posts <- searchFacebook(string = "neoptide ducray", token, n = 500)
pages <- searchPages( string="ducray", token, n=100)
post <- getPost(post_id, token, n = 1000, likes = TRUE, comments = TRUE)
me <- getUsers("lpfrenchie",token)
post_id <- head(pages$id, n = 2)  ## ID of most recent post
post <- getPost(post_id, token)
users <- getUsers(post$likes$from_id, token)
