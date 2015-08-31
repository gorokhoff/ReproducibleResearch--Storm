dsm<-ds[, c("EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")]

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

dsm$EVTYPE<-stri_trim_both(dsm$EVTYPE)

dsm$EVTYPE<-stri_replace_all(dsm$EVTYPE, regex =  "[/|\\(|\\)|\\\\|&|\\.]", " ")
dsm$EVTYPE<-stri_replace_all_fixed(dsm$EVTYPE, "-", " ")
dsm$EVTYPE<-stri_replace_all_fixed(dsm$EVTYPE, "/", " ")
dsm$EVTYPE<-stri_replace_all_fixed(dsm$EVTYPE, "AND", "")
dsm$EVTYPE<-stri_replace_all_fixed(dsm$EVTYPE, "SLIDES", "SLIDE")
dsm$EVTYPE<-stri_replace_all_fixed(dsm$EVTYPE, "FLOODS", "FLOOD")
dsm$EVTYPE<-stri_replace_all_fixed(dsm$EVTYPE, "THUNDERSTORMS", "THUNDERSTORM")
dsm$EVTYPE<-stri_replace_all_fixed(dsm$EVTYPE, "THUNDERESTORM", "THUNDERSTORM")
dsm$EVTYPE<-stri_replace_all_fixed(dsm$EVTYPE, "THUNDEERSTORM", "THUNDERSTORM")
dsm$EVTYPE<-stri_replace_all_fixed(dsm$EVTYPE, "THUDERSTORM", "THUNDERSTORM")
dsm$EVTYPE<-stri_replace_all_fixed(dsm$EVTYPE, "THUNDERSTORMW", "THUNDERSTORM")
dsm$EVTYPE<-stri_replace_all_fixed(dsm$EVTYPE, "THUNDERTORM", "THUNDERSTORM")
dsm$EVTYPE<-stri_replace_all_fixed(dsm$EVTYPE, "THUNDERSTORMWIND", "THUNDERSTORM WIND")
dsm$EVTYPE<-stri_replace_all_fixed(dsm$EVTYPE, "THUNERSTORM", "THUNDERSTORM")
dsm$EVTYPE<-stri_replace_all_fixed(dsm$EVTYPE, "THUNDERSTROM", "THUNDERSTORM")
dsm$EVTYPE<-stri_replace_all_fixed(dsm$EVTYPE, "THUNDERSTORMINDS", "THUNDERSTORM WIND")

dsm$EVTYPE<-stri_replace_all_fixed(dsm$EVTYPE, "WINDS", "WIND")
dsm$EVTYPE<-stri_replace_all_fixed(dsm$EVTYPE, "WINS", "WIND")
dsm$EVTYPE<-stri_replace_all_fixed(dsm$EVTYPE, "FLOODING", "FLOOD")
dsm$EVTYPE<-stri_replace_all_fixed(dsm$EVTYPE, "  "," ")
dsm$EVTYPE<-stri_replace_all_fixed(dsm$EVTYPE, "FLD", "FLOOD")
dsm$EVTYPE<-stri_replace_all_fixed(dsm$EVTYPE, "THUNDERSTORM WINDS", "THUNDERSTORM WIND")
dsm$EVTYPE<-stri_replace_all_fixed(dsm$EVTYPE, "FLOOD FLASHFLOOD", "FLASHFLOOD")
dsm$EVTYPE<-stri_replace_all_fixed(dsm$EVTYPE, "FLASH FLOOD FLOOD", "FLASHFLOOD")
dsm$EVTYPE<-stri_replace_all_fixed(dsm$EVTYPE, "FLASH FLOOD", "FLASHFLOOD")
dsm$EVTYPE<-stri_replace_all_fixed(dsm$EVTYPE, "FLOOD FLASH FLOOD", "FLASHFLOOD")
dsm$EVTYPE<-stri_replace_all_fixed(dsm$EVTYPE, "FLOOD FLASHFLOOD", "FLASHFLOOD")
dsm$EVTYPE<-stri_replace_all_fixed(dsm$EVTYPE, "FLOOD FLASH", "FLASHFLOOD")
dsm$EVTYPE<-stri_replace_all_fixed(dsm$EVTYPE, "RAINS", "RAIN")
dsm$EVTYPE<-stri_replace_all_fixed(dsm$EVTYPE, "CURRENTS", "CURRENT")
dsm$EVTYPE<-stri_replace_all_fixed(dsm$EVTYPE, "CURRENTS", "CURRENT")
dsm$EVTYPE<-stri_replace_all_fixed(dsm$EVTYPE, "LIGNTNING", "LIGHTNING")
dsm$EVTYPE<-stri_replace_all_fixed(dsm$EVTYPE, "LIGHTING", "LIGHTNING")
dsm$EVTYPE<-stri_replace_all_fixed(dsm$EVTYPE, "MUDSLIDE", "MUD SLIDE")
dsm$EVTYPE<-stri_replace_all_fixed(dsm$EVTYPE, "STORMS", "STORM")
dsm$EVTYPE<-stri_replace_all_fixed(dsm$EVTYPE, "FIRES", "FIRE")
dsm$EVTYPE<-stri_replace_all_fixed(dsm$EVTYPE, "WILDFIRE", "WILD FIRE")
dsm$EVTYPE<-stri_replace_all_fixed(dsm$EVTYPE, "SML", "SMALL")

dsm$EVTYPE<-stri_replace_all_fixed(dsm$EVTYPE, "ICY", "ICE")
dsm$EVTYPE<-stri_replace_all_fixed(dsm$EVTYPE, "  "," ")
dsm$EVTYPE<-stri_trim_both(dsm$EVTYPE)

dsm$EVTYPE<-as.factor(dsm$EVTYPE)
prdmg<-aggregate(PROPDMGVAL ~ EVTYPE, data = dsm, FUN = sum)
prdmg<-subset(prdmg, PROPDMGVAL > 0)
cropdmg<-aggregate(CROPDMGVAL ~ EVTYPE, data = dsm, FUN = sum)
cropdmg<-subset(cropdmg, CROPDMGVAL > 0)
prdmg$EVTYPE<-as.factor(prdmg$EVTYPE)


fat<-aggregate(FATALITIES ~ EVTYPE, data = dsm, FUN = sum)
fat<-subset(fat, FATALITIES > 0)
inj<-aggregate(INJURIES ~ EVTYPE, data = dsm, FUN = sum)
inj<-subset(inj, INJURIES > 0)

fat<-fat[order(-fat$FATALITIES),]
inj<-inj[order(-inj$INJURIES),]
prdmg<-prdmg[order(-prdmg$PROPDMGVAL),]
cropdmg<-cropdmg[order(-cropdmg$CROPDMGVAL),]
topFat<-fat[1:10,]
barplot(topFat$FATALITIES, las = 3, names.arg = topFat$EVTYPE, main = "Weather Events With The Top 10 Highest Fatalities",     ylab = "Number of fatalities", col = "red")
