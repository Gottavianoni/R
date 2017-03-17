#necessary file for Windows
#download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")

library(twitteR)

#to get your consumerKey and consumerSecret see the twitteR documentation for instructions
consumer_key <- "x15MznTyTC9leJR9W9NB3vkV3"
consumer_secret <- "IGKnNywt9hfpevtzv0ljZJb4jJjvZTjVhtY4kcHgVNLPOquhlU"
access_token <- "119530363-6bkYUkXvJFjVcs5nbsNBrFX7KcFb3T8Glh6EqziD"
access_secret <- "slNuLVeGkIMNBBWZKvotDovCL1SR7pwLLZAXGfVQcbnNI"
setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_secret)

r_stats <- searchTwitter("galenic", lang = "fr", n = 2000)
x <- twListToDF(twList = r_stats)
avis <- paste(x[,1],collapse = " ")

install.packages("Rfacebook")
library(Rfacebook)

token <- "CAACEdEose0cBAAi3k5cAhxl422pKjMjZAZC203KfxaqGUlGTHgoIPixn799kyW9knsGnqw055xaPXiMEdzbv3C7zDCcHt9a561Gb4tqlqZCe9bZCN0c9ZA20E8UOS1bDxMHCiIKV6xvxt2UpmDhqRFgZBsWYZBwy7QWgZCD69388wCMRGw1JZC3E5uUI9mkEZBaFMZD"
posts <- searchFacebook(string = "neoptide ducray", token, n = 500)
pages <- searchPages( string="ducray", token, n=100)
post <- getPost(post_id, token, n = 1000, likes = TRUE, comments = TRUE)
me <- getUsers("lpfrenchie",token)
post_id <- head(pages$id, n = 2)  ## ID of most recent post
post <- getPost(post_id, token)
users <- getUsers(post$likes$from_id, token)
