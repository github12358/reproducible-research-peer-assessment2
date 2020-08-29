# Analytis United States, which types of events are most harmful and most economic consequences.

## Reproducible Research: Peer-graded Assignment: Course Project 2

### 1. Assignment

The basic goal of this assignment is to explore the NOAA Storm Database and answer some basic questions about severe weather events. You must use the database to answer the questions below and show the code for your entire analysis. Your analysis can consist of tables, figures, or other summaries. You may use any R package you want to support your analysis.

### 2. Data

The data for this assignment come in the form of a comma-separated-value file compressed via the bzip2 algorithm to reduce its size. You can download the file from the course web site
There is also some documentation of the database available. Here you will find how some of the variables are constructed/defined.

National Weather Service Storm Data Documentation
National Climatic Data Center Storm Events FAQ


### 3. Data Processing


##### 3.1. Load source file and extract it
Import Dataset
```


##### 3.2. Load the data
Read the source .csv file

```r
if(!"weatherdata" %in% ls()) {
        weatherdata <- read.csv("datafile.csv.bz2")
}
```

##### 3.3. Load required libraries

```r
library(ggplot2)
```

##### 3.4.Create Data Frame for event type, fatalities and injuries, Create data frame for event type, property damage and crop damage

```r
weatherdataclean <- data.frame(weatherdata$EVTYPE,weatherdata$FATALITIES, weatherdata$INJURIES)
colnames(weatherdataclean) = c("EVTYPE", "FATALITIES", "INJURIES")

damagedataclean <- data.frame(weatherdata$EVTYPE,weatherdata$PROPDMG, weatherdata$PROPDMGEXP, weatherdata$CROPDMG, weatherdata$CROPDMGEXP)

colnames(damagedataclean) = c("EVTYPE", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")
```

##### 3.5. Create new metric for combined property + crop damage
(K = 1,000, M = 1,000,000, B = 1,000,000,000)

```r
damagedataclean$PROPDMGMult <- ifelse (damagedataclean$PROPDMGEXP == "K", 1000, ifelse (damagedataclean$PROPDMGEXP == "M", 1000000, ifelse (damagedataclean$PROPDMGEXP == "B", 1000000000, 0)))

damagedataclean$PROPDMGAMT <- damagedataclean$PROPDMG*damagedataclean$PROPDMGMult

damagedataclean$CROPDMGMult <- ifelse (damagedataclean$CROPDMGEXP == "K", 1000, ifelse (damagedataclean$CROPDMGEXP == "M", 1000000, ifelse (damagedataclean$CROPDMGEXP == "B", 1000000000, 0)))

damagedataclean$CROPDMGAMT <- damagedataclean$CROPDMG*damagedataclean$CROPDMGMult

damagedataclean$TOTALDMGAMT <- damagedataclean$PROPDMGAMT+damagedataclean$CROPDMGAMT
```

##### 3.6. Refactor EVTYPE into 11 levels
The EVTYPE contains ca. 985 unique source events. Many of them can be reduced to similar instances. 
In this instance there are 11 levels defined, covering effectifly the majority and all useful data records (summaries and combinations are skipped)

```r
if(dataProcess){
  reducedStormData$damageSource <- NA
  
  reducedStormData[grepl("precipitation|rain|hail|drizzle|wet|percip|burst|depression|fog|wall cloud", 
                         reducedStormData$EVTYPE, ignore.case = TRUE), "damageSource"] <- "Precipitation & Fog"
  reducedStormData[grepl("wind|storm|wnd|hurricane|typhoon", 
                         reducedStormData$EVTYPE, ignore.case = TRUE), "damageSource"] <- "Wind & Storm"
  reducedStormData[grepl("slide|erosion|slump", 
                         reducedStormData$EVTYPE, ignore.case = TRUE), "damageSource"] <- "Landslide & Erosion"
  reducedStormData[grepl("warmth|warm|heat|dry|hot|drought|thermia|temperature record|record temperature|record high", 
                         reducedStormData$EVTYPE, ignore.case = TRUE), "damageSource"] <- "Heat & Drought"
  reducedStormData[grepl("cold|cool|ice|icy|frost|freeze|snow|winter|wintry|wintery|blizzard|chill|freezing|avalanche|glaze|sleet", 
                         reducedStormData$EVTYPE, ignore.case = TRUE), "damageSource"] <- "Snow & Ice"
  reducedStormData[grepl("flood|surf|blow-out|swells|fld|dam break", 
                         reducedStormData$EVTYPE, ignore.case = TRUE), "damageSource"] <- "Flooding & High Surf"
  reducedStormData[grepl("seas|high water|tide|tsunami|wave|current|marine|drowning", 
                         reducedStormData$EVTYPE, ignore.case = TRUE), "damageSource"] <- "High seas"
  reducedStormData[grepl("dust|saharan", 
                         reducedStormData$EVTYPE, ignore.case = TRUE), "damageSource"] <- "Dust & Saharan winds"  
  reducedStormData[grepl("tstm|thunderstorm|lightning", 
                         reducedStormData$EVTYPE, ignore.case = TRUE), "damageSource"] <- "Thunderstorm & Lightning"
  reducedStormData[grepl("tornado|spout|funnel|whirlwind", 
                         reducedStormData$EVTYPE, ignore.case = TRUE), "damageSource"] <- "Tornado"
  reducedStormData[grepl("fire|smoke|volcanic", 
                         reducedStormData$EVTYPE, ignore.case = TRUE), "damageSource"] <- "Fire & Volcanic activity"
  
  # remove uncategorized records (damageSource == NA) & cast as factor
  reducedStormData <- reducedStormData[complete.cases(reducedStormData[, "damageSource"]), ]
  reducedStormData$damageSource <- as.factor(reducedStormData$damageSource)
  
  # clean reducedStormData
  reducedStormData$EVTYPE <- NULL
}
```

### 4. Results
##### 4.1. Fatalities
Only the top 10 events are shown

```r
weatherfatalities <- aggregate(weatherdataclean$FATALITIES, by = list(weatherdataclean$EVTYPE), FUN = sum, na.rm = TRUE)
colnames(weatherfatalities) = c("EVTYPE", "FATALITIES")
weatherfatalities <- weatherfatalities[order(-weatherfatalities$FATALITIES),]
topweatherfatalities <- weatherfatalities[1: 10, ]

p<- ggplot(topweatherfatalities, aes(x=reorder(EVTYPE, FATALITIES), y=FATALITIES))
p+geom_bar(stat = "identity", fill = "red")+ ggtitle("Top 10 Weather Events by # Fatalities")+labs(x = "Event Type", y="#Fatalities") +theme(axis.text.x = element_text(angle=45, hjust=1)) 
```
![types of events](https://user-images.githubusercontent.com/69575052/91639643-3c61b980-ea42-11ea-915e-b9fbc2216c4d.PNG) 

```
From the information shown above, Tornados are the most harmful events to population health based on total number fatalities.
```

##### 4.2. Injuries 
 Only the top 10 events are shown

```r
weatherinjury <- aggregate(weatherdataclean$INJURIES, by = list(weatherdataclean$EVTYPE), FUN = sum, na.rm = TRUE)
colnames(weatherinjury) = c("EVTYPE", "INJURIES")
weatherinjury <- weatherinjury[order(-weatherinjury$INJURIES),]
topweatherinjury <- weatherinjury[1: 10, ]

q<- ggplot(topweatherinjury, aes(x=reorder(EVTYPE, INJURIES), y=INJURIES))
q+geom_bar(stat = "identity", fill = "blue")+ ggtitle("Top 10 Weather Events by # Injuries")+labs(x = "Event Type", y="#Injuries") +theme(axis.text.x = element_text(angle=45, hjust=1)) 
```
![Injuries](https://user-images.githubusercontent.com/69575052/91639593-f4429700-ea41-11ea-92e5-c7145e97e100.PNG)

```
From the information shown above, Tornados are the most harmful events to population health based on total number injuries.
```

##### 4.3. Economic consequences
Only the top 10 events are shown

```r
TOTALDMGAMT <- aggregate(damagedataclean$TOTALDMGAMT, by = list(damagedataclean$EVTYPE), FUN = sum, na.rm = TRUE)
colnames(TOTALDMGAMT) = c("EVTYPE", "TOTALDMGAMT")
TOTALDMGAMT <- TOTALDMGAMT[order(-TOTALDMGAMT$TOTALDMGAMT),]
TOPTOTALDMGAMT <- TOTALDMGAMT[1: 10, ]

r<- ggplot(TOPTOTALDMGAMT, aes(x=reorder(EVTYPE, TOTALDMGAMT/1000000000), y=TOTALDMGAMT/1000000000))
r+geom_bar(stat = "identity", fill = "green")+ ggtitle("Top 10 Weather Events by Total Damage (in $ Billions)")+labs(x = "Event Type", y="Total Damage (in $ Billions)") +theme(axis.text.x = element_text(angle=45, hjust=1)) 
```
![economic consequences](https://user-images.githubusercontent.com/69575052/91639692-8e0a4400-ea42-11ea-8e9e-574d25cdcf15.PNG)

```
From the information shown above, Floods have the greatest economic consequences based on total dollars of property and crop damage.
```

###Conclusion
```
Tornado is the most harmful weather event in the U.S with respect to population health.

Floods have the greatest economic consequences in the U.S.
```
