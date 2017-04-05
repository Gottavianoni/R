#MAIL
install.packages("RDCOMClient")
library(RDCOMClient)
## init com api
OutApp <- COMCreate("Outlook.Application")
## create an email 
outMail = OutApp$CreateItem(0)
## configure  email parameter 
outMail[["To"]] = "*****"
outMail[["subject"]] = "some subject"
outMail[["body"]] = "some body"
## send it                     
outMail$Send()
outMail[["Attachments"]]$Add(path_to_attch_file)
