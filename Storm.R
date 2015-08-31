if(!"repdata-data-StormData.csv.bz2" %in% dir("./")){
  download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", destfile = "repdata-data-StormData.csv.bz2")
}
if(!"repdata-data-StormData.csv" %in% dir("./")){
  unzip("repdata-data-StormData.csv.bz2")
}
if(!"ds" %in% ls()){
  ds<-read.csv("repdata-data-StormData.csv")
}
dsm<-ds[, c("EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")]
dim(unique(dsm$PROPDMGEXP))
dsm$PROPEXP[dsm$PROPDMGEXP == "K"] <- 1000
dsm$PROPEXP[dsm$PROPDMGEXP == "k"] <- 1000
dsm$PROPEXP[dsm$PROPDMGEXP == "M"] <- 1e+06
dsm$PROPEXP[dsm$PROPDMGEXP == ""] <- 1
dsm$PROPEXP[dsm$PROPDMGEXP == "B"] <- 1e+09
dsm$PROPEXP[dsm$PROPDMGEXP == "m"] <- 1e+06
dsm$PROPEXP[dsm$PROPDMGEXP == "0"] <- 1
dsm$PROPEXP[dsm$PROPDMGEXP == "5"] <- 1e+05
dsm$PROPEXP[dsm$PROPDMGEXP == "6"] <- 1e+06
dsm$PROPEXP[dsm$PROPDMGEXP == "4"] <- 10000
dsm$PROPEXP[dsm$PROPDMGEXP == "2"] <- 100
dsm$PROPEXP[dsm$PROPDMGEXP == "3"] <- 1000
dsm$PROPEXP[dsm$PROPDMGEXP == "h"] <- 100
dsm$PROPEXP[dsm$PROPDMGEXP == "7"] <- 1e+07
dsm$PROPEXP[dsm$PROPDMGEXP == "H"] <- 100
dsm$PROPEXP[dsm$PROPDMGEXP == "1"] <- 10
dsm$PROPEXP[dsm$PROPDMGEXP == "8"] <- 1e+08 
# give 0 to invalid exponent data, so they not count in
dsm$PROPEXP[dsm$PROPDMGEXP == "+"] <- 0
dsm$PROPEXP[dsm$PROPDMGEXP == "-"] <- 0
dsm$PROPEXP[dsm$PROPDMGEXP == "?"] <- 0 
# compute the property damage value
dsm$PROPDMGVAL <- dsm$PROPDMG * dsm$PROPEXP
#steps.by.date <- aggregate(steps ~ date, data = data, FUN = sum)
levels(dsm$CROPDMGEXP)
dsm$CROPEXP[dsm$CROPDMGEXP == "M"] <- 1e+06
dsm$CROPEXP[dsm$CROPDMGEXP == "K"] <- 1000
dsm$CROPEXP[dsm$CROPDMGEXP == "m"] <- 1e+06
dsm$CROPEXP[dsm$CROPDMGEXP == "B"] <- 1e+09
dsm$CROPEXP[dsm$CROPDMGEXP == "0"] <- 1
dsm$CROPEXP[dsm$CROPDMGEXP == "k"] <- 1000
dsm$CROPEXP[dsm$CROPDMGEXP == "2"] <- 100
dsm$CROPEXP[dsm$CROPDMGEXP == ""] <- 1 
# give 0 to invalid exponent data, so they not count in
dsm$CROPEXP[dsm$CROPDMGEXP == "?"] <- 0 

dsm$PROPDMGVAL <- dsm$PROPDMG * dsm$PROPEXP
dsm$CROPDMGVAL <- dsm$CROPDMG * dsm$CROPEXP

dsm$EVTYPE<-toupper(dsm$EVTYPE)
library(stringi)
dsm$EVTYPE<-stri_trim_both(dsm$EVTYPE)
dsm$EVTYPE<-stri_replace_all_fixed(dsm$EVTYPE, "/", " ")
dsm$EVTYPE<-stri_replace_all_fixed(dsm$EVTYPE, "(", "")
dsm$EVTYPE<-stri_replace_all_fixed(dsm$EVTYPE, ")", "")
dsm$EVTYPE<-stri_replace_all_fixed(dsm$EVTYPE, "  "," ")
dsm$EVTYPE<-stri_replace_all_fixed(dsm$EVTYPE, "WINDS", "WIND")


dsm$EVTYPE<-as.factor(dsm$EVTYPE)
prdmg<-aggregate(PROPDMGVAL ~ EVTYPE, data = dsm, FUN = sum)
cropdmg<-aggregate(CROPDMGVAL ~ EVTYPE, data = dsm, FUN = sum)

#MUD SLIDE  6.001000e+05
#	MUD SLIDES
#ss<-subset(ds, EVTYPE == "Summary of June 15")
ss<-subset(ds, EVTYPE == "Summary of June 15")
prdmgsub<-subset(prdmg, PROPDMGVAL > 0)
