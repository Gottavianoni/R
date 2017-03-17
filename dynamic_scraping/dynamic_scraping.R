#SANS PROXY POUR UTILISER LE PHANTOMJS
Sys.setenv(https_proxy = "")
Sys.setenv(http_proxy = "")

## Not run: 
# start the server if one isnt running
startServer()

# use default server initialisation values
remDr <- remoteDriver$new()

# send request to server to initialise session
remDr$open()

# navigate to R home page
remDr$navigate("http://www.r-project.org")

# navigate to www.bbc.co.uk notice the need for http://
remDr$navigate("http://www.bbc.co.uk")

# go backwards and forwards
remDr$goBack()

remDr$goForward()

remDr$goBack()

proxy <- "http://advmalb:2Deploi$3Cde@proxy1:8080"
proxy <- "http://ottavig:Pierre-Fabre03@proxy1:8080"

require(RSelenium)
require(rvest)
phantom(pjs_cmd = "C:/Users/ottavig/Documents/phantomjs.exe",extra = "--proxy-auth=ottavig:Pierre-Fabre03 --proxy=proxy1:8080")
remDr <- remoteDriver(browserName = "phantomjs")
remDr$open()
remDr$navigate("http://www.ordre.pharmacien.fr/annuaire/pharmacien")
x <- html(remDr$getPageSource()[[1]]) %>% html_text()
elt <- remDr$findElement(using = "css selector", value = "input#etablissement")
elt$clickElement()

path <- "div.navbar-form.navbar-left"
elt <- remDr$findElement(using = "css selector" , value = path)
elt$sendKeysToElement(code_postal[1])
path <- "div.navbar-form.navbar-left div + button"
remDr$findElement(using = "css selector" , value = path)$clickElement()


remDr$findElement(using = "css selector" , value = 'div input.submit')$clickElement()

x <- html(remDr$getPageSource()[[1]])
x %>% html_nodes("id") %>% html_attrs()

remDr$navigate("http://ariya.github.com/js/random/")
remDr$findElement("id", "numbers")$getElementText()[[1]] # Math.random returns our custom function
remDr$close()
remDr$stop()
 
#FINESS

phantom(pjs_cmd = "C:/Users/ottavig/Documents/phantomjs.exe", extra = "--proxy-auth=ottavig:Pierre-Fabre02 --proxy=proxy1:8080")
remDr <- remoteDriver(browserName = "phantomjs")
remDr$open()
remDr$navigate("http://finess.sante.gouv.fr/jsp/index.jsp")
x <- html(remDr$getPageSource()[[1]])
elt <- remDr$findElement(using = "css selector", value = 'a[accesskey="2"]')
elt$clickElement()
elt <- remDr$findElement(using = "css selector", value = "input#idtest")
elt$clickElement()

elt2 <- remDr$findElement(using = "css selector", value = "")
elt$clickElement()

#CNOM
phantom(pjs_cmd = "C:/Users/ottavig/Documents/phantomjs.exe", extra = "--proxy-auth=ottavig:Pierre-Fabre02 --proxy=proxy1:8080")
remDr <- remoteDriver(browserName = "phantomjs")
remDr$open()
remDr$navigate("http://finess.sante.gouv.fr/jsp/index.jsp")
x <- html(remDr$getPageSource()[[1]]) %>% html_text()

elt <- remDr$findElement(using = "css selector", value = 'a[accesskey="2"]')
elt$clickElement()

elt <- remDr$findElement(using = "css selector", value = "select#idtest")
remDr$findElement(using = "css selector", value = "select#region option[value='42']")$clickElement()
remDr$findElement(using = "css selector", value = "select#dep option[value='67']")$clickElement()
remDr$findElement(using = "css selector", value = "select#commune option[value='001']")$clickElement()
remDr$findElement(using = "css selector", value = "input#ajoutChoixLoc")$clickElement()
remDr$findElement(using = "css selector", value = "a[tabindex='234']")$clickElement()
remDr$findElement(using = "css selector", value = "input[value='620']")$clickElement()
remDr$findElement(using = "css selector", value = "input#chercher")$clickElement()

elt <- remDr$findElement(using = "css selector", value = "select#commune")
elt$sendKeysToElement(list("BRESSOLS"))

elt$submitElement()

#ORDRE DES MEDECINS

Sys.setenv(http_proxy = '')
require(RSelenium)
require(rvest)
phantom(pjs_cmd = "C:/Users/ottavig/Documents/phantomjs.exe",extra = "--proxy-auth=ottavig:Pierre-Fabre03 --proxy=proxy1:8080")
remDr <- remoteDriver(browserName = "phantomjs")
remDr$open()
 
remDr$navigate("http://fcci/#/")

remDr$navigate("http://www.conseil-national.medecin.fr/annuaire")
remDr$findElement(using = "css selector", value = "div[class='form-item form-type-select form-item-discipline'] button")$clickElement()
remDr$findElement(using = "css selector", value = "li[data-original-index='32'] a")$clickElement()
 
remDr$findElement(using = "css selector", value = "input#edit-sexe-3")$clickElement()

remDr$findElement(using = "css selector", value = "div[class='form-item form-type-select form-item-departement'] button")$clickElement()
remDr$findElement(using = "css selector", value = "div[class='form-item form-type-select form-item-departement'] li[data-original-index='3'] a")$clickElement()

remDr$findElement(using = "css selector", value = "input#submit_adv_search")$clickElement()

#Accepter les termes du contrat

remDr$findElement(using = "css selector", value = "span#recaptcha-anchor")$clickElement()
remDr$findElement(using = "css selector", value = "div.captcha div.g-recaptcha iframe")$clickElement()
remDr$findElement(using = "css selector", value = "div.captcha div.g-recaptcha iframe")$submitElement()

remDr$findElement(using = "css selector", value = "input#edit-op")$clickElement()

elt$submitElement()
remDr$getCurrentUrl()
x <- html(enc2native(remDr$getPageSource()[[1]])) %>% html_nodes("div.captcha div.g-recaptcha iframe div")