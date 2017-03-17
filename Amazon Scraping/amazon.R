#MAIN
proxy <- "http://ottavig:Pierre-Fabre03@proxy1:8080";
Sys.setenv(http_proxy = proxy);
Sys.setenv(https_proxy = proxy);
source("C:/users/ottavig/Documents/Script_R/Amazon Scraping/amazon_scraping.R",encoding = "UTF-8")
source("C:/users/ottavig/Documents/Script_R/Amazon Scraping/amazon_formatage.R",encoding = "UTF-8")