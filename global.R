# globals
library(jsonlite)
# Dropdowns
states<-read.csv("external/States.csv")
counties<-read.csv("external/County_dropdown.csv")
hucs<-read.csv("external/8DigitHUCs.csv", colClasses = c(HUC8 = "character"))
load('external/countydt.rdata')
#characteristics<-fromJSON("http://www.waterqualitydata.us/Codes/Characteristicname?mimeType=json")$codes
#charTypes<-fromJSON("http://www.waterqualitydata.us/Codes/Characteristictype?mimeType=json")$codes
#sample_media<-fromJSON("http://www.waterqualitydata.us/Codes/Samplemedia?mimeType=json")$codes
#siteType<-fromJSON("http://www.waterqualitydata.us/Codes/Sitetype?mimeType=json")$codes
#ORGID<-fromJSON("http://www.waterqualitydata.us/Codes/Organization?mimeType=json")$codes






