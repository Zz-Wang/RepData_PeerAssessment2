#Reproducible Assignment 2
#Zhizheng Wang
#2014.12

#load the data
#url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
setwd("C:/Users/User/Documents/R/rep")
storm_full <- read.csv("repdata-data-StormData.csv",  header = TRUE)

my_data <- data.frame(storm_full$EVTYPE, storm_full$FATALITIES, storm_full$INJURIES, 
                      storm_full$PROPDMG, storm_full$PROPDMGEXP, storm_full$CROPDMG, 
                      storm_full$CROPDMGEXP)

#top 10 fatality/injury event(classed by different type)
library(plyr)
cdata <- ddply(my_data, c("storm_full.EVTYPE"), summarise,
               sum_fatal  = sum(storm_full.FATALITIES, na.rm=TRUE),
               sum_injury = sum(storm_full.INJURIES, na.rm=TRUE))
colnames(cdata) = c("event.type", "fatality.total", "injury.total")
cdata.sorted1 <- cdata[order(-cdata$fatality.total),] 
topcdata.fatality <- cdata.sorted1[1:10,]
cdata.sorted2 <- cdata[order(-cdata$injury.total),] 
topcdata.injury <- cdata.sorted2[1:10,]

par(mfrow=c(2,1))
barinfo1<-barplot(topcdata.fatality$fatality.total, names.arg = topcdata.fatality$event.type,
                  space = 0.4,  xlab="Event Type", ylab="Numbers Killed", 
                  main="Most Deadly Natural Disasters", col="lightgreen")
barinfo2<-barplot(topcdata.injury$injury.total, names.arg = topcdata.injury$event.type, 
                  space = 0.4,  xlab="Event Type", ylab="Numbers Injured", 
                  main="Most Deadly Natural Disasters II", col="lightgreen")

#top disasters with high economic loss
table(my_data$storm_full.PROPDMGEXP)
table(my_data$storm_full.CROPDMGEXP)
my_data$storm_full.PROPDMGEXP <- as.character(my_data$storm_full.PROPDMGEXP)
my_data$storm_full.CROPDMGEXP <- as.character(my_data$storm_full.CROPDMGEXP)

my_data$storm_full.PROPDMGEXP[(my_data$storm_full.PROPDMGEXP == "")] <- 0
my_data$storm_full.PROPDMGEXP[(my_data$storm_full.PROPDMGEXP == "+") | (my_data$storm_full.PROPDMGEXP == 
                                                          "-") | (my_data$storm_full.PROPDMGEXP == "?")] <- 1
my_data$storm_full.PROPDMGEXP[(my_data$storm_full.PROPDMGEXP == "h") | (my_data$storm_full.PROPDMGEXP == 
                                                          "H")] <- 2
my_data$storm_full.PROPDMGEXP[(my_data$storm_full.PROPDMGEXP == "k") | (my_data$storm_full.PROPDMGEXP == 
                                                          "K")] <- 3
my_data$storm_full.PROPDMGEXP[(my_data$storm_full.PROPDMGEXP == "m") | (my_data$storm_full.PROPDMGEXP == 
                                                          "M")] <- 6
my_data$storm_full.PROPDMGEXP[(my_data$storm_full.PROPDMGEXP == "B")] <- 9

my_data$storm_full.CROPDMGEXP[(my_data$storm_full.CROPDMGEXP == "")] <- 0
my_data$storm_full.CROPDMGEXP[(my_data$storm_full.CROPDMGEXP == "+") | (my_data$storm_full.CROPDMGEXP == 
                                                          "-") | (my_data$storm_full.CROPDMGEXP == "?")] <- 1
my_data$storm_full.CROPDMGEXP[(my_data$storm_full.CROPDMGEXP == "h") | (my_data$storm_full.CROPDMGEXP == 
                                                          "H")] <- 2
my_data$storm_full.CROPDMGEXP[(my_data$storm_full.CROPDMGEXP == "k") | (my_data$storm_full.CROPDMGEXP == 
                                                          "K")] <- 3
my_data$storm_full.CROPDMGEXP[(my_data$storm_full.CROPDMGEXP == "m") | (my_data$storm_full.CROPDMGEXP == 
                                                          "M")] <- 6
my_data$storm_full.CROPDMGEXP[(my_data$storm_full.CROPDMGEXP == "B")] <- 9

# convert to integer for computation
my_data$storm_full.PROPDMGEXP <- as.integer(my_data$storm_full.PROPDMGEXP)
my_data$storm_full.CROPDMGEXP <- as.integer(my_data$storm_full.CROPDMGEXP)

my_data$damage_total <- my_data$storm_full.PROPDMG * 10 ^ my_data$storm_full.PROPDMGEXP +
  my_data$storm_full.CROPDMG * 10 ^ my_data$storm_full.CROPDMGEXP
ddata <- ddply(my_data, c("storm_full.EVTYPE"), summarise,
               sum_damage_total  = sum(damage_total, na.rm=TRUE))
colnames(ddata) = c("event.type", "damage_total")
ddata.sorted <- ddata[order(-ddata$damage_total),] 
topddata.damage <- ddata.sorted[1:10,]

par(mfrow=c(1,1))
barinfo3<-barplot(height = topddata.damage$damage_total, names.arg = topddata.damage$event.type, 
                  space = 0.4,  xlab="Event Type", ylab="Economic Loss (Dollar)", 
                  main="Most Economically Damaged Disasters", col="lightgreen")
