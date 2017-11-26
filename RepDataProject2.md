# Weather events with the greatest impact on public health and economics in the U.S.
Christian Romo  
25/11/2017  

# Synopsis

Nowadays humanity has a better understanding and even being able to predict meteorological events, but 
weather is still powerful force we have keep an eye on. Tornadoes and hurricanes are two examples of 
severe weather capable of causing loss of life and major damage. Snowstorms trap people in their homes 
or businesses, forcing them to wait out and paralyzing entire cities. Even severe rain can have a major 
effect on society through transportation delays.

This document explores the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database
to find out what weather events have the greatest impact on population and economics.


# Data Processing

#### Loading required libraries


```r
    require(dplyr)
```

```
## Loading required package: dplyr
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
    require(lubridate)
```

```
## Loading required package: lubridate
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```r
    require(stringr)
```

```
## Loading required package: stringr
```

```r
    require(stringdist)
```

```
## Loading required package: stringdist
```

```r
    require(ggplot2)
```

```
## Loading required package: ggplot2
```

```r
    require(cowplot)
```

```
## Loading required package: cowplot
```

```
## 
## Attaching package: 'cowplot'
```

```
## The following object is masked from 'package:ggplot2':
## 
##     ggsave
```

```r
    require(xtable)
```

```
## Loading required package: xtable
```

#### Data reading

Data can be dowloaded from [here.](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2)

Reading the file with all the storm information, also reading a file that contains the names for
the 48 types of events.

```r
    stormData <- read.csv(file = "repdata_StormData.csv.bz2")
    eventTypes <- read.csv("EventTypes.csv", col.names = c("event"), header = FALSE)
```

#### Data cleaning

Removing unnecesary columns. 

```r
    stormData <- stormData[, c("BGN_DATE", "EVTYPE", "FATALITIES", "INJURIES", 
                               "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")]
```

Selecting columns with relevant data about damage, fatalities or injuries. 

```r
    stormData <- stormData[stormData$FATALITIES > 0 | stormData$INJURIES > 0 
                           | stormData$PROPDMG > 0 | stormData$CROPDMG > 0, ]
```

Analyzing event types recorded by year.

```r
    stormData$YEAR <- year(as.Date(as.character(stormData$BGN_DATE), format = "%m/%d/%Y"))
```

```
## Warning in strptime(x, format, tz = "GMT"): unknown timezone 'zone/tz/
## 2017c.1.0/zoneinfo/America/Mexico_City'
```

```r
    byYearData <- group_by(stormData, YEAR) %>% summarise(EVENTSNUM = length(unique(EVTYPE)))
```

Even when the EVTYPE column is not normalized, we can see that the most of the types of events began
being recorded from 1993.

```r
    # Year with at least 48 diferent type of events recorded
    xTable <- xtable(byYearData[byYearData$EVENTSNUM >= 48, ])
    print(xTable, type = "html")
```

<!-- html table generated in R 3.4.2 by xtable 1.8-2 package -->
<!-- Sat Nov 25 16:25:06 2017 -->
<table border=1>
<tr> <th>  </th> <th> YEAR </th> <th> EVENTSNUM </th>  </tr>
  <tr> <td align="right"> 1 </td> <td align="right"> 1993.00 </td> <td align="right"> 107 </td> </tr>
  <tr> <td align="right"> 2 </td> <td align="right"> 1994.00 </td> <td align="right"> 158 </td> </tr>
  <tr> <td align="right"> 3 </td> <td align="right"> 1995.00 </td> <td align="right"> 210 </td> </tr>
  <tr> <td align="right"> 4 </td> <td align="right"> 1996.00 </td> <td align="right">  88 </td> </tr>
  <tr> <td align="right"> 5 </td> <td align="right"> 1997.00 </td> <td align="right">  97 </td> </tr>
  <tr> <td align="right"> 6 </td> <td align="right"> 1998.00 </td> <td align="right">  69 </td> </tr>
  <tr> <td align="right"> 7 </td> <td align="right"> 1999.00 </td> <td align="right">  63 </td> </tr>
  <tr> <td align="right"> 8 </td> <td align="right"> 2000.00 </td> <td align="right">  62 </td> </tr>
  <tr> <td align="right"> 9 </td> <td align="right"> 2001.00 </td> <td align="right">  60 </td> </tr>
  <tr> <td align="right"> 10 </td> <td align="right"> 2002.00 </td> <td align="right">  54 </td> </tr>
   </table>

```r
    # Data recorded before 1993
    xTable2 <- xtable(byYearData[byYearData$YEAR <= 1993, ])
    print(xTable2, type = "html")
```

<!-- html table generated in R 3.4.2 by xtable 1.8-2 package -->
<!-- Sat Nov 25 16:25:07 2017 -->
<table border=1>
<tr> <th>  </th> <th> YEAR </th> <th> EVENTSNUM </th>  </tr>
  <tr> <td align="right"> 1 </td> <td align="right"> 1950.00 </td> <td align="right">   1 </td> </tr>
  <tr> <td align="right"> 2 </td> <td align="right"> 1951.00 </td> <td align="right">   1 </td> </tr>
  <tr> <td align="right"> 3 </td> <td align="right"> 1952.00 </td> <td align="right">   1 </td> </tr>
  <tr> <td align="right"> 4 </td> <td align="right"> 1953.00 </td> <td align="right">   1 </td> </tr>
  <tr> <td align="right"> 5 </td> <td align="right"> 1954.00 </td> <td align="right">   1 </td> </tr>
  <tr> <td align="right"> 6 </td> <td align="right"> 1955.00 </td> <td align="right">   1 </td> </tr>
  <tr> <td align="right"> 7 </td> <td align="right"> 1956.00 </td> <td align="right">   1 </td> </tr>
  <tr> <td align="right"> 8 </td> <td align="right"> 1957.00 </td> <td align="right">   1 </td> </tr>
  <tr> <td align="right"> 9 </td> <td align="right"> 1958.00 </td> <td align="right">   1 </td> </tr>
  <tr> <td align="right"> 10 </td> <td align="right"> 1959.00 </td> <td align="right">   1 </td> </tr>
  <tr> <td align="right"> 11 </td> <td align="right"> 1960.00 </td> <td align="right">   1 </td> </tr>
  <tr> <td align="right"> 12 </td> <td align="right"> 1961.00 </td> <td align="right">   1 </td> </tr>
  <tr> <td align="right"> 13 </td> <td align="right"> 1962.00 </td> <td align="right">   1 </td> </tr>
  <tr> <td align="right"> 14 </td> <td align="right"> 1963.00 </td> <td align="right">   1 </td> </tr>
  <tr> <td align="right"> 15 </td> <td align="right"> 1964.00 </td> <td align="right">   1 </td> </tr>
  <tr> <td align="right"> 16 </td> <td align="right"> 1965.00 </td> <td align="right">   1 </td> </tr>
  <tr> <td align="right"> 17 </td> <td align="right"> 1966.00 </td> <td align="right">   1 </td> </tr>
  <tr> <td align="right"> 18 </td> <td align="right"> 1967.00 </td> <td align="right">   1 </td> </tr>
  <tr> <td align="right"> 19 </td> <td align="right"> 1968.00 </td> <td align="right">   1 </td> </tr>
  <tr> <td align="right"> 20 </td> <td align="right"> 1969.00 </td> <td align="right">   1 </td> </tr>
  <tr> <td align="right"> 21 </td> <td align="right"> 1970.00 </td> <td align="right">   1 </td> </tr>
  <tr> <td align="right"> 22 </td> <td align="right"> 1971.00 </td> <td align="right">   1 </td> </tr>
  <tr> <td align="right"> 23 </td> <td align="right"> 1972.00 </td> <td align="right">   1 </td> </tr>
  <tr> <td align="right"> 24 </td> <td align="right"> 1973.00 </td> <td align="right">   1 </td> </tr>
  <tr> <td align="right"> 25 </td> <td align="right"> 1974.00 </td> <td align="right">   1 </td> </tr>
  <tr> <td align="right"> 26 </td> <td align="right"> 1975.00 </td> <td align="right">   1 </td> </tr>
  <tr> <td align="right"> 27 </td> <td align="right"> 1976.00 </td> <td align="right">   1 </td> </tr>
  <tr> <td align="right"> 28 </td> <td align="right"> 1977.00 </td> <td align="right">   1 </td> </tr>
  <tr> <td align="right"> 29 </td> <td align="right"> 1978.00 </td> <td align="right">   1 </td> </tr>
  <tr> <td align="right"> 30 </td> <td align="right"> 1979.00 </td> <td align="right">   1 </td> </tr>
  <tr> <td align="right"> 31 </td> <td align="right"> 1980.00 </td> <td align="right">   1 </td> </tr>
  <tr> <td align="right"> 32 </td> <td align="right"> 1981.00 </td> <td align="right">   1 </td> </tr>
  <tr> <td align="right"> 33 </td> <td align="right"> 1982.00 </td> <td align="right">   1 </td> </tr>
  <tr> <td align="right"> 34 </td> <td align="right"> 1983.00 </td> <td align="right">   2 </td> </tr>
  <tr> <td align="right"> 35 </td> <td align="right"> 1984.00 </td> <td align="right">   3 </td> </tr>
  <tr> <td align="right"> 36 </td> <td align="right"> 1985.00 </td> <td align="right">   3 </td> </tr>
  <tr> <td align="right"> 37 </td> <td align="right"> 1986.00 </td> <td align="right">   3 </td> </tr>
  <tr> <td align="right"> 38 </td> <td align="right"> 1987.00 </td> <td align="right">   3 </td> </tr>
  <tr> <td align="right"> 39 </td> <td align="right"> 1988.00 </td> <td align="right">   3 </td> </tr>
  <tr> <td align="right"> 40 </td> <td align="right"> 1989.00 </td> <td align="right">   3 </td> </tr>
  <tr> <td align="right"> 41 </td> <td align="right"> 1990.00 </td> <td align="right">   3 </td> </tr>
  <tr> <td align="right"> 42 </td> <td align="right"> 1991.00 </td> <td align="right">   3 </td> </tr>
  <tr> <td align="right"> 43 </td> <td align="right"> 1992.00 </td> <td align="right">   3 </td> </tr>
  <tr> <td align="right"> 44 </td> <td align="right"> 1993.00 </td> <td align="right"> 107 </td> </tr>
   </table>

As we need the most of the types of events for a true comparison, we'll only be analyzing data 
from 1993.

```r
    # Also removing unnnecesary columns
    stormData <- stormData[stormData$YEAR >= 1993 , c("EVTYPE", "FATALITIES", "INJURIES", 
                               "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")]
```

Normalizing EVTYPE data.

```r
    # Removing whitespaces and transforming text to upper case
    stormData$EVTYPE <- toupper(trimws(stormData$EVTYPE))
    # Removing empty events
    stormData <- stormData[stormData$EVTYPE != "?", ]
    # Removing all SUMMARY events
    stormData <- stormData[!grepl("SUMMARY|MONTHLY|MONTH|YEAR|MAY|OTHER", x = stormData$EVTYPE), ]
    # Removing numbers and other unnecesary chars
    stormData$EVTYPE <- str_replace_all(stormData$EVTYPE, "[0123456789().]", "")
    # Removing extra whitespaces
    stormData$EVTYPE <- gsub("\\s\\s", " ", stormData$EVTYPE)
    # Removing unnecesary words
    removeWords <- c("SEVERE|RECORD|PATCHY|GUSTY|FIRST|LATE|UNSEASONABLY|SEASON|NON|ABNORMALLY",
                        "|ROGUE|FALLING|PROLONGED|ACCUMULATED|PROLONG|UNUSUALLY|ROUGH|LOCALLY|UNUSUAL",
                        "|ABNORMAL|MOUNTAIN|METRO|RAPIDLY|LACK|LARGE|MAJOR|MPH|DAMAGE|LOW|EARLY",
                        "|MODERATE|BITTER|MIXED|LOCAL|RURAL|GRADIENT|STREAM|NEAR|BING|SHOWERS|MINOR",
                        "|WAVE|NORMAL|AGRICULTURAL|SML|STREET|SHOWER|PLUME|SAHARAN|AIR|RIVER|OPAL",
                        "|DRY|PATTERN|ACCUMULATION|MIX|CONDITIONS|GENERATED|SWELLS|TEMPREATURES",
                        "|TEMPERATURE|TEMPS|TEMP|BREAKUP|WET|SQUALLS|ALOFT|ADVISORY|DRIFTING",
                        "|SNOWMELT|HIGHWAY|ROTATING|INJURY|JAM|GROUND|ADVISORIES|SMALL")
    stormData$EVTYPE <- gsub(removeWords, "", stormData$EVTYPE)
```

```
## Warning in gsub(removeWords, "", stormData$EVTYPE): el argumento 'pattern'
## tiene tiene longitud > 1 y s'olo el primer elemento ser'a usado
```

```r
    stormData$EVTYPE <- trimws(stormData$EVTYPE)
    # Fixing typos, abbreviations and synonyms
    stormData$EVTYPE <- gsub("TSTM|THUNERSTORM|THUNDERSTROM|THUNDERSTORMS|THUNDERSTORMW|THUNDERTORM|RAINSTORM|STORM FORCE",
                             "THUNDERSTORM", stormData$EVTYPE)
    stormData$EVTYPE <- gsub("FLOODING|FLD|FLDG|FLOODIN|RISING WATER", "FLOOD", stormData$EVTYPE)
    stormData$EVTYPE <- gsub("TORNADOES|TORNDAO|TORNADOS", "TORNADO", stormData$EVTYPE)
    stormData$EVTYPE <- gsub("WND|WINDS", "WIND", stormData$EVTYPE)
    stormData$EVTYPE <- gsub("CSTL|TIDAL", "COASTAL", stormData$EVTYPE)
    stormData$EVTYPE <- gsub("RAINFALL|PRECIPITATION|PRECIPATATION|PRECIP", "RAIN", stormData$EVTYPE)
    stormData$EVTYPE <- gsub("HVY|TORRENTIAL", "HEAVY", stormData$EVTYPE)
    stormData$EVTYPE <- gsub("AVALANCE", "AVALANCHE", stormData$EVTYPE)
    stormData$EVTYPE <- gsub("SURFING", "SURF", stormData$EVTYPE)
    stormData$EVTYPE <- gsub("TYPHOON", "HURRICANE", stormData$EVTYPE)
    stormData$EVTYPE <- gsub("DEVEL", "DEVIL", stormData$EVTYPE)
    stormData$EVTYPE <- gsub("DUSTSTORM|DUST STORM", "DUST DEVIL", stormData$EVTYPE)
    stormData$EVTYPE <- gsub("ERUPTION|ASHFALL", "ASH", stormData$EVTYPE)
    stormData$EVTYPE <- gsub("HOT|WARMTH|WARM|HEAT SPELL|SPELL", "HEAT", stormData$EVTYPE)
    stormData$EVTYPE <- gsub("LIGNTNING", "LIGHTNING", stormData$EVTYPE)
    stormData$EVTYPE <- gsub("EXTENDED", "EXTREME", stormData$EVTYPE)
    stormData$EVTYPE <- gsub("SNOWPACK|SNOWSTORM|SNOWFALL", "SNOW", stormData$EVTYPE)
    stormData$EVTYPE <- gsub("WINTERY", "WINTER", stormData$EVTYPE)
    stormData$EVTYPE <- gsub("ICESTORM", "ICE STORM", stormData$EVTYPE)
    stormData$EVTYPE <- gsub("WHIRLWIND", "STRONG WIND", stormData$EVTYPE)
    stormData$EVTYPE <- gsub("BRUSH FIRES", "WILDFIRE", stormData$EVTYPE)
    stormData$EVTYPE <- gsub("WALL", "FUNNEL", stormData$EVTYPE)
    stormData$EVTYPE <- gsub("WATER SPOUT|WAYTERSPOUT", "WATERSPOUT", stormData$EVTYPE)
    # Replacing chars and extra whitespaces for / when needed
    stormData$EVTYPE <- gsub(";\\s|\\s;|AND|\\sAND\\s|\\s\\s/|\\s/|/\\s\\s|/\\s|\\s&\\s|\\s&|,\\s|\\\\", 
                             "/", stormData$EVTYPE)
    stormData$EVTYPE <- trimws(stormData$EVTYPE)
    # Removing extra whitespaces
    stormData$EVTYPE <- gsub("\\s/|^-\\s|^-|/$|^/|-$|;$", "", stormData$EVTYPE)
    # Removing single chars left after replacing
    stormData$EVTYPE <- gsub("\\s[A-Z]{1}$|^[A-Z]{1}\\s", "", stormData$EVTYPE)
    stormData$EVTYPE <- gsub("\\s\\s", " ", stormData$EVTYPE)
    # Removing empty events left after cleaning
    stormData <- stormData[stormData$EVTYPE != "", ]
```

Calculating the number of unique type of events.

```r
    numEvents <- length(unique(stormData$EVTYPE))
```
After cleaning event names there are 268 diferent types of events.


Function for matching the event names with the official types of events.

```r
    matchEventType <- function(event) {
        match <- 0
        newEvent <- ""
        # Matching text with amatch function
        match <- amatch(event, eventTypes$event, maxDist = 10, nomatch = 0)
            if(match > 0) {
                newEvent <- eventTypes[match, "event"]
            } else {
                newEvent <- event
                # if no first match, then split string by / when multiple events reported
                # and take the first matched string
                # if no match again, then return original event name
                words <- strsplit(event, "/")[[1]]
                if(length(words) > 1) {
                    for(word in words) {
                        match <- amatch(word, eventTypes$event, maxDist = 10, nomatch = 0)
                        if(match > 0) {
                            newEvent <- eventTypes[match, "event"]
                            break;
                        } 
                    }
                }
            }
        as.character(newEvent)
    }
```

Matching the event names with the official types of events.

```r
    stormData$EVTYPE <- mapply(matchEventType, stormData$EVTYPE)
```

Calculating the number of unique type of events.

```r
    numEvents <- length(unique(stormData$EVTYPE))
```
After matching event names there are 52 unique type of events.

Selecting only the rows that match with the official types of events.

```r
    match <- stormData$EVTYPE %in% eventTypes$event
    stormData <-  stormData[match, ]
    noMatch <- length(which(!match))
```
Removing 6 events that couldn't be matched.

The dimensions of the final data to be analized.

```r
    dim(stormData)
```

[1] 227201      7

#### Calculating population health damage.

```r
    stormData$TOTALPOPDMG <- stormData$FATALITIES + stormData$INJURIES
```

#### Calculating economic damage.

Function for calculating total economic damage.

```r
    calculateTotalDamage <- function(propDmg, propEx, cropDmg, cropEx) {
        exp <- c(D = 10, H = 100, K = 1000, M = 1000000, B = 1000000000)
        totalDmg <- 0
        totalPropDmg <-  0
        totalCropDmg <-  0
        propEx <- as.character(propEx)
        cropEx <- as.character(cropEx)
        # calculating property damage
        if(propDmg > 0 & propEx != "" & propEx != "-" & propEx != "?" & propEx != "+") {
            propEx <- toupper(propEx)
            if(propEx >= 0 & propEx <= 8) {
                propEx <- "D"
            }
            totalPropDmg <- propDmg * as.numeric(exp[propEx])
        }
        # calculating crop damage
        if(cropDmg > 0 & cropEx != "" & cropEx != "-" & cropEx != "?" & cropEx != "+") {
            cropEx <- toupper(cropEx)
            if(cropEx >= 0 & cropEx <= 8) {
                cropEx <- "D"
            }
            totalCropDmg <- cropDmg * as.numeric(exp[cropEx])
        }
        totalDmg <- totalPropDmg + totalCropDmg
        totalDmg
    }
```

Calculating economic damage.

```r
    stormData$TOTALECODMG <- mapply(calculateTotalDamage, stormData$PROPDMG, stormData$PROPDMGEXP, 
                                 stormData$CROPDMG, stormData$CROPDMGEXP)
```

#### Summary of population damage.

```r
    populationDmg <- stormData[ stormData$TOTALPOPDMG > 0, ]
```

Grouping population damage data by event type and calculating total damage.

```r
    populationTotals <- group_by(populationDmg, EVTYPE) %>% summarise(TOTAL = sum(TOTALPOPDMG))
```

Plotting population damage data.

```r
    ggplot(populationTotals , aes(EVTYPE, TOTAL, fill = EVTYPE)) + 
    geom_bar(stat = "identity") + coord_flip() + 
    theme(legend.position="none", axis.text.y = element_text(size = 7), 
          axis.text.x = element_text(size = 7)) + 
    labs(title = "Total population damage by event type") +
    labs(x = "Event type", y = "Total fatalities and injuries")
```

![](RepDataProject2_files/figure-html/populationD-1.png)<!-- -->

#### Summary of economic damage.

```r
    economicDmg <- stormData[ stormData$TOTALECODMG > 0, ]
```

Grouping economic damage data by event type and calculating total damage (in millions).

```r
    economicTotals <- group_by(economicDmg, EVTYPE) %>% summarise(TOTAL = sum(TOTALECODMG))
    economicTotals$TOTAL <- economicTotals$TOTAL / 1000000
```

Plotting economic damage data.

```r
    ggplot(economicTotals , aes(EVTYPE, TOTAL, fill = EVTYPE)) + 
    geom_bar(stat = "identity") + coord_flip() + 
    theme(legend.position="none", axis.text.y = element_text(size = 7), 
          axis.text.x = element_text(size = 7)) + 
    labs(title = "Total economic damage by event type") +
    labs(x = "Event type", y = "Total damage (millions of dollars)")
```

![](RepDataProject2_files/figure-html/economicD-1.png)<!-- -->

# Results

#### Ordering data by total damage of each event type.

Ordering population damage data by the most harmful events (Event name, Total fatalities and injuries).

```r
    populationTotals <- populationTotals[order(-populationTotals$TOTAL), ]
    xTable <- xtable(populationTotals)
    print(xTable, type = "html")
```

<!-- html table generated in R 3.4.2 by xtable 1.8-2 package -->
<!-- Sat Nov 25 16:51:27 2017 -->
<table border=1>
<tr> <th>  </th> <th> EVTYPE </th> <th> TOTAL </th>  </tr>
  <tr> <td align="right"> 1 </td> <td> TORNADO </td> <td align="right"> 24974.00 </td> </tr>
  <tr> <td align="right"> 2 </td> <td> EXCESSIVE HEAT </td> <td align="right"> 8723.00 </td> </tr>
  <tr> <td align="right"> 3 </td> <td> FLOOD </td> <td align="right"> 8568.00 </td> </tr>
  <tr> <td align="right"> 4 </td> <td> THUNDERSTORM WIND </td> <td align="right"> 6647.00 </td> </tr>
  <tr> <td align="right"> 5 </td> <td> LIGHTNING </td> <td align="right"> 6051.00 </td> </tr>
  <tr> <td align="right"> 6 </td> <td> HEAT </td> <td align="right"> 3816.00 </td> </tr>
  <tr> <td align="right"> 7 </td> <td> FLASH FLOOD </td> <td align="right"> 2946.00 </td> </tr>
  <tr> <td align="right"> 8 </td> <td> ICE STORM </td> <td align="right"> 2118.00 </td> </tr>
  <tr> <td align="right"> 9 </td> <td> HIGH WIND </td> <td align="right"> 1804.00 </td> </tr>
  <tr> <td align="right"> 10 </td> <td> WILDFIRE </td> <td align="right"> 1698.00 </td> </tr>
  <tr> <td align="right"> 11 </td> <td> WINTER STORM </td> <td align="right"> 1554.00 </td> </tr>
  <tr> <td align="right"> 12 </td> <td> HURRICANE </td> <td align="right"> 1466.00 </td> </tr>
  <tr> <td align="right"> 13 </td> <td> HEAVY SNOW </td> <td align="right"> 1341.00 </td> </tr>
  <tr> <td align="right"> 14 </td> <td> RIP CURRENT </td> <td align="right"> 1106.00 </td> </tr>
  <tr> <td align="right"> 15 </td> <td> HAIL </td> <td align="right"> 1016.00 </td> </tr>
  <tr> <td align="right"> 16 </td> <td> BLIZZARD </td> <td align="right"> 906.00 </td> </tr>
  <tr> <td align="right"> 17 </td> <td> WINTER WEATHER </td> <td align="right"> 606.00 </td> </tr>
  <tr> <td align="right"> 18 </td> <td> DUST DEVIL </td> <td align="right"> 507.00 </td> </tr>
  <tr> <td align="right"> 19 </td> <td> TROPICAL STORM </td> <td align="right"> 449.00 </td> </tr>
  <tr> <td align="right"> 20 </td> <td> STRONG WIND </td> <td align="right"> 414.00 </td> </tr>
  <tr> <td align="right"> 21 </td> <td> FUNNEL CLOUD </td> <td align="right"> 397.00 </td> </tr>
  <tr> <td align="right"> 22 </td> <td> AVALANCHE </td> <td align="right"> 395.00 </td> </tr>
  <tr> <td align="right"> 23 </td> <td> HEAVY RAIN </td> <td align="right"> 362.00 </td> </tr>
  <tr> <td align="right"> 24 </td> <td> DENSE FOG </td> <td align="right"> 360.00 </td> </tr>
  <tr> <td align="right"> 25 </td> <td> HIGH SURF </td> <td align="right"> 311.00 </td> </tr>
  <tr> <td align="right"> 26 </td> <td> SEICHE </td> <td align="right"> 271.00 </td> </tr>
  <tr> <td align="right"> 27 </td> <td> EXTREME COLD/WIND CHILL </td> <td align="right"> 171.00 </td> </tr>
  <tr> <td align="right"> 28 </td> <td> TSUNAMI </td> <td align="right"> 162.00 </td> </tr>
  <tr> <td align="right"> 29 </td> <td> COLD/WIND CHILL </td> <td align="right"> 107.00 </td> </tr>
  <tr> <td align="right"> 30 </td> <td> WATERSPOUT </td> <td align="right"> 78.00 </td> </tr>
  <tr> <td align="right"> 31 </td> <td> STORM SURGE/TIDE </td> <td align="right"> 67.00 </td> </tr>
  <tr> <td align="right"> 32 </td> <td> MARINE THUNDERSTORM WIND </td> <td align="right"> 54.00 </td> </tr>
  <tr> <td align="right"> 33 </td> <td> FREEZING FOG </td> <td align="right"> 49.00 </td> </tr>
  <tr> <td align="right"> 34 </td> <td> MARINE STRONG WIND </td> <td align="right"> 36.00 </td> </tr>
  <tr> <td align="right"> 35 </td> <td> DROUGHT </td> <td align="right"> 32.00 </td> </tr>
  <tr> <td align="right"> 36 </td> <td> COASTAL FLOOD </td> <td align="right"> 20.00 </td> </tr>
  <tr> <td align="right"> 37 </td> <td> MARINE HAIL </td> <td align="right"> 15.00 </td> </tr>
  <tr> <td align="right"> 38 </td> <td> MARINE HIGH WIND </td> <td align="right"> 6.00 </td> </tr>
  <tr> <td align="right"> 39 </td> <td> SLEET </td> <td align="right"> 6.00 </td> </tr>
   </table>
There are events with little to no impact on population healt, for the purpouse of this document we will 
be usign the top five events with the greatest impact on population health.

Ordering economic damage data by events with the greatest economic consequences (Event name, total damage in millions of dollars).

```r
    economicTotals <- economicTotals[order(-economicTotals$TOTAL), ]
    xTable <- xtable(economicTotals)
    print(xTable, type = "html")
```

<!-- html table generated in R 3.4.2 by xtable 1.8-2 package -->
<!-- Sat Nov 25 16:52:13 2017 -->
<table border=1>
<tr> <th>  </th> <th> EVTYPE </th> <th> TOTAL </th>  </tr>
  <tr> <td align="right"> 1 </td> <td> FLOOD </td> <td align="right"> 161277.00 </td> </tr>
  <tr> <td align="right"> 2 </td> <td> HURRICANE </td> <td align="right"> 90762.58 </td> </tr>
  <tr> <td align="right"> 3 </td> <td> STORM SURGE/TIDE </td> <td align="right"> 47965.58 </td> </tr>
  <tr> <td align="right"> 4 </td> <td> TORNADO </td> <td align="right"> 28361.30 </td> </tr>
  <tr> <td align="right"> 5 </td> <td> HAIL </td> <td align="right"> 18786.76 </td> </tr>
  <tr> <td align="right"> 6 </td> <td> FLASH FLOOD </td> <td align="right"> 18531.69 </td> </tr>
  <tr> <td align="right"> 7 </td> <td> HEAT </td> <td align="right"> 15440.55 </td> </tr>
  <tr> <td align="right"> 8 </td> <td> THUNDERSTORM WIND </td> <td align="right"> 12307.47 </td> </tr>
  <tr> <td align="right"> 9 </td> <td> ICE STORM </td> <td align="right"> 9215.05 </td> </tr>
  <tr> <td align="right"> 10 </td> <td> WILDFIRE </td> <td align="right"> 8894.40 </td> </tr>
  <tr> <td align="right"> 11 </td> <td> TROPICAL STORM </td> <td align="right"> 8409.29 </td> </tr>
  <tr> <td align="right"> 12 </td> <td> WINTER STORM </td> <td align="right"> 6716.94 </td> </tr>
  <tr> <td align="right"> 13 </td> <td> HIGH WIND </td> <td align="right"> 6678.25 </td> </tr>
  <tr> <td align="right"> 14 </td> <td> HEAVY RAIN </td> <td align="right"> 4047.40 </td> </tr>
  <tr> <td align="right"> 15 </td> <td> FROST/FREEZE </td> <td align="right"> 1428.00 </td> </tr>
  <tr> <td align="right"> 16 </td> <td> FUNNEL CLOUD </td> <td align="right"> 1386.11 </td> </tr>
  <tr> <td align="right"> 17 </td> <td> HEAVY SNOW </td> <td align="right"> 1098.43 </td> </tr>
  <tr> <td align="right"> 18 </td> <td> LIGHTNING </td> <td align="right"> 948.39 </td> </tr>
  <tr> <td align="right"> 19 </td> <td> BLIZZARD </td> <td align="right"> 771.37 </td> </tr>
  <tr> <td align="right"> 20 </td> <td> EXCESSIVE HEAT </td> <td align="right"> 649.21 </td> </tr>
  <tr> <td align="right"> 21 </td> <td> SLEET </td> <td align="right"> 487.88 </td> </tr>
  <tr> <td align="right"> 22 </td> <td> COASTAL FLOOD </td> <td align="right"> 450.15 </td> </tr>
  <tr> <td align="right"> 23 </td> <td> SEICHE </td> <td align="right"> 359.87 </td> </tr>
  <tr> <td align="right"> 24 </td> <td> STRONG WIND </td> <td align="right"> 251.59 </td> </tr>
  <tr> <td align="right"> 25 </td> <td> TSUNAMI </td> <td align="right"> 144.08 </td> </tr>
  <tr> <td align="right"> 26 </td> <td> MARINE HIGH WIND </td> <td align="right"> 111.30 </td> </tr>
  <tr> <td align="right"> 27 </td> <td> HIGH SURF </td> <td align="right"> 90.87 </td> </tr>
  <tr> <td align="right"> 28 </td> <td> WATERSPOUT </td> <td align="right"> 60.74 </td> </tr>
  <tr> <td align="right"> 29 </td> <td> WINTER WEATHER </td> <td align="right"> 42.30 </td> </tr>
  <tr> <td align="right"> 30 </td> <td> LAKE-EFFECT SNOW </td> <td align="right"> 40.18 </td> </tr>
  <tr> <td align="right"> 31 </td> <td> EXTREME COLD/WIND CHILL </td> <td align="right"> 26.50 </td> </tr>
  <tr> <td align="right"> 32 </td> <td> FREEZING FOG </td> <td align="right"> 13.60 </td> </tr>
  <tr> <td align="right"> 33 </td> <td> DUST DEVIL </td> <td align="right"> 9.92 </td> </tr>
  <tr> <td align="right"> 34 </td> <td> ASTRONOMICAL LOW TIDE </td> <td align="right"> 9.74 </td> </tr>
  <tr> <td align="right"> 35 </td> <td> DENSE FOG </td> <td align="right"> 9.67 </td> </tr>
  <tr> <td align="right"> 36 </td> <td> LAKESHORE FLOOD </td> <td align="right"> 7.54 </td> </tr>
  <tr> <td align="right"> 37 </td> <td> DROUGHT </td> <td align="right"> 6.86 </td> </tr>
  <tr> <td align="right"> 38 </td> <td> MARINE THUNDERSTORM WIND </td> <td align="right"> 5.92 </td> </tr>
  <tr> <td align="right"> 39 </td> <td> AVALANCHE </td> <td align="right"> 3.72 </td> </tr>
  <tr> <td align="right"> 40 </td> <td> COLD/WIND CHILL </td> <td align="right"> 2.59 </td> </tr>
  <tr> <td align="right"> 41 </td> <td> MARINE STRONG WIND </td> <td align="right"> 2.12 </td> </tr>
  <tr> <td align="right"> 42 </td> <td> TROPICAL DEPRESSION </td> <td align="right"> 1.74 </td> </tr>
  <tr> <td align="right"> 43 </td> <td> DUST STORM </td> <td align="right"> 0.60 </td> </tr>
  <tr> <td align="right"> 44 </td> <td> VOLCANIC ASH </td> <td align="right"> 0.50 </td> </tr>
  <tr> <td align="right"> 45 </td> <td> RIP CURRENT </td> <td align="right"> 0.16 </td> </tr>
  <tr> <td align="right"> 46 </td> <td> DENSE SMOKE </td> <td align="right"> 0.10 </td> </tr>
  <tr> <td align="right"> 47 </td> <td> MARINE HAIL </td> <td align="right"> 0.05 </td> </tr>
   </table>
There are events with little to no economic damage, for the purpouse of this document we will be usign 
the top five events with the greatest economic consequences.


#### Comparison of the events with greatest population and economic consequences.

```r
    plot1 <- ggplot(head(populationTotals, 5) , aes(x = reorder(EVTYPE, -TOTAL), TOTAL, fill = EVTYPE)) + 
    geom_bar(stat = "identity") + coord_flip() + 
    theme(legend.position="none", axis.text.y = element_text(size = 7), 
          axis.text.x = element_text(size = 7), 
          plot.title = element_text(size = 8), axis.title = element_text(size = 8)) + 
    labs(title = "Total population damage by event type") +
    labs(x = "Event type", y = "Total fatalities and injuries")

    plot2 <- ggplot(head(economicTotals, 5) , aes(x = reorder(EVTYPE, -TOTAL), TOTAL, fill = EVTYPE)) + 
    geom_bar(stat = "identity") + coord_flip() + 
    theme(legend.position="none", axis.text.y = element_text(size = 7), 
          axis.text.x = element_text(size = 7), 
          plot.title = element_text(size = 8), axis.title = element_text(size = 8)) + 
    labs(title = "Total economic damage by event type") +
    labs(x = "Event type", y = "Total damage (millions of dollars)")
    
    plot_grid(plot1, plot2)
```

![](RepDataProject2_files/figure-html/resultsC-1.png)<!-- -->

We can see that the top five events with the greatest impact on population health are:

1. Tornado
2. Excessive heat
3. Flood
4. Thunderstorm wind
5. Lightning

We can see that the top five events with the greatest economic consequences are:

1. Flood
2. Hurricane
3. Storm surge/tide
4. Tornado
5. Hail

Even with a better understanding of climate and the capability to predict meteorological events, we have to prepared to react immediatly when the probabilities for this ten events to happen are not in our favor, so we can limit the potential damage, at least in population health.
