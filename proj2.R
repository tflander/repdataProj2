
library(plyr)
library(stringr)
library(knitr)

zipFile <- "repdata-data-StormData.csv.bz2"
if(!file.exists(zipFile)) { # skip download if we already have the zip file
  message("downloading data...")
  dataLink <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
  download.file(dataLink, destfile=zipFile, method="curl")
}

if(!exists("rawData")) {
  message("loading data...")
  rawData <<- read.csv(bzfile(zipFile))  
  
  ## clean the event type column
  rawData$eventType <- str_trim(toupper(rawData$EVTYPE))
  
  ## create clean data set
  eventTypeCol <- 38
  # drop data with no ill effects
  cleanData <- rawData[rawData$PROPDMG + rawData$CROPDMG + rawData$FATALITIES + rawData$INJURIES > 0,]
  # rm(rawData)
  
  #generalize events
  generalizeEvent <- function(pattern, generalEvent) {
    cleanData[grepl(pattern, cleanData$eventType), eventTypeCol] <- generalEvent
    cleanData
  }

  # Important -- do not reorder
  cleanData <- generalizeEvent("HAIL", "THUNDERSTORM/WIND/HAIL")
  cleanData <- generalizeEvent("^TROPICAL STORM", "TROPICAL STORM")
  cleanData <- generalizeEvent("FLOOD", "FLOOD/WET")
  cleanData <- generalizeEvent("EXCESSIVE WETNESS", "FLOOD/WET")
  cleanData <- generalizeEvent("COOL AND WET", "FLOOD/WET")
  cleanData <- generalizeEvent("WET MICROBURST", "FLOOD/WET")
  cleanData <- generalizeEvent("DUST DEVIL", "WATERSPOUT/TORNADO/DUST DEVIL")
  cleanData <- generalizeEvent("WATERSPOUT", "WATERSPOUT/TORNADO/DUST DEVIL")
  cleanData <- generalizeEvent("LANDSPOUT", "WATERSPOUT/TORNADO/DUST DEVIL")
  cleanData <- generalizeEvent("TORNADO", "WATERSPOUT/TORNADO/DUST DEVIL")
  cleanData <- generalizeEvent("SNOW", "RAIN/SNOW/ICE")
  cleanData <- generalizeEvent("ICE", "THUNDERSTORM/RAIN/SNOW/ICE")
  cleanData <- generalizeEvent("COLD", "COLD/FREEZE")
  cleanData <- generalizeEvent("WIND", "THUNDERSTORM/WIND/HAIL")
  cleanData <- generalizeEvent("RAIN", "RAIN/SNOW/ICE")  
  cleanData <- generalizeEvent("WINTER", "RAIN/SNOW/ICE")  
  cleanData <- generalizeEvent("THUNDERSTORM", "THUNDERSTORM/WIND/HAIL")
  cleanData <- generalizeEvent("^\\?$", "UNKNOWN/OTHER")  
  cleanData <- generalizeEvent("OTHER", "UNKNOWN/OTHER")  
  cleanData <- generalizeEvent("FIRE", "FIRE")  
  cleanData <- generalizeEvent("DENSE SMOKE", "FIRE")  
  cleanData <- generalizeEvent("DUST", "THUNDERSTORM/WIND/HAIL")
  cleanData <- generalizeEvent("PRECIPITATION", "RAIN/SNOW/ICE")  
  cleanData <- generalizeEvent("FROST", "COLD/FREEZE")
  cleanData <- generalizeEvent("FREEZ", "COLD/FREEZE")
  cleanData <- generalizeEvent("LANDSL", "LANDSLIDE")
  cleanData <- generalizeEvent("MIX", "RAIN/SNOW/ICE")  
  cleanData <- generalizeEvent("DOWNBURST", "RAIN/SNOW/ICE")
  cleanData <- generalizeEvent("STORM", "THUNDERSTORM/WIND/HAIL")
  cleanData <- generalizeEvent("WATER", "FLOOD/WET")
  cleanData <- generalizeEvent("SHOWER", "THUNDERSTORM/WIND/HAIL")
  cleanData <- generalizeEvent("LOW TEMPERATURE", "COLD/FREEZE")
  cleanData <- generalizeEvent("HEAT", "HEAT")
  cleanData <- generalizeEvent("HURRICANE", "HURRICANE")
  cleanData <- generalizeEvent("BLIZZARD", "RAIN/SNOW/ICE")
  cleanData <- generalizeEvent("^LIG", "LIGHTNING")
  cleanData <- generalizeEvent("^MUD", "MUDSLIDE")
  cleanData <- generalizeEvent("LANDSLIDE", "LANDSLIDE/MUDSLIDE")
  cleanData <- generalizeEvent("FUNNEL CLOUD", "WATERSPOUT/TORNADO/DUST DEVIL")
  cleanData <- generalizeEvent("TORNDAO", "WATERSPOUT/TORNADO/DUST DEVIL")
  cleanData <- generalizeEvent("ICY ROADS", "RAIN/SNOW/ICE")
  cleanData <- generalizeEvent("GLAZE", "RAIN/SNOW/ICE")
  cleanData <- generalizeEvent("GUSTNADO", "THUNDERSTORM/WIND/HAIL")
  cleanData <- generalizeEvent("STREAM", "FLOOD/WET")
  cleanData <- generalizeEvent("SURF", "SURF")
  
  # unique(cleanData$eventType[grepl("WIND", cleanData$eventType)])
  
  baseColsToSave <- c(
    2, # BGN_DATE
    7, # STATE
    8, # EV_TYPE
    11, # BGN_LOCATI
    36, # REMARKS
    38 # eventType
  )
  
  injuriesColsToSave <- c(
    23, # FATALITIES
    24 # INJURIES
  )
  
  damageColsToSave <- c(
    25, # PROPDMG
    26, # PROPDMGEXP
    27, # CROPDMG
    28 # CROPDMGEXP  
  )
  
  ## Split raw data into two data frames.  One for Injuries and Deaths, and the other for Damage.
  deathsAndInjuries <<- cleanData[cleanData$INJURIES + cleanData$FATALITIES > 0, c(baseColsToSave, injuriesColsToSave)]
  damage <<- cleanData[cleanData$PROPDMG + cleanData$CROPDMG > 0, c(baseColsToSave, damageColsToSave)]
  
  ## clean the damage unit explination column
  damage$PROPDMGEXP <- toupper(damage$PROPDMGEXP)
  damage$CROPDMGEXP <- toupper(damage$CROPDMGEXP)
  damage$CROPDMGEXP[damage$CROPDMGEXP=="?"] <- ""

  ## define a function to create a multiplier column based on the unit explination column
  computeMultiplier <- function(expCol, amtCol, multCol) {
    damage[multCol] <- 0
    damage[multCol][damage[expCol] == "H"] <- .1
    damage[multCol][damage[expCol] == "K"] <- 1
    damage[multCol][damage[expCol] == "M"] <- 1000
    damage[multCol][damage[expCol] == "B"] <- 1000000
    suppressWarnings(
      damage[[multCol]][!is.na(as.numeric(damage[[expCol]]))] <- as.numeric(damage[[expCol]])[!is.na(as.numeric(damage[[expCol]]))]
    )
    
    # Assume ones if invalid chars
    damage[multCol][damage[multCol]==0 & damage[amtCol]] <- .001
    damage
  }

  ## add the damage multiplier column to the damage data frame
  damage$propMultiplier <- 0
  damage$cropMultiplier <- 0

  ## document columns for computing actual damage
  propDmgAmtCol <- 7
  cropDmgAmtCol <- 9
  propDmgExpCol <- 8
  cropDmgExpCol <- 10
  propMultiplierCol <- 11
  cropMultiplierCol <- 12

  ## compute multiplier for determining actual damage
  damage <- computeMultiplier(propDmgExpCol, propDmgAmtCol, propMultiplierCol)
  damage <- computeMultiplier(cropDmgExpCol, cropDmgAmtCol, cropMultiplierCol)

  ## add columns for actual damage in thousands
  damage$propDmgThousands <- damage$propMultiplier * damage$PROPDMG
  damage$cropDmgThousands <- damage$cropMultiplier * damage$CROPDMG
  
} # end data load

## Summarize Death / Injury Data
healthByEventType <- ddply(deathsAndInjuries, .(eventType), numcolwise(sum))
fatalities <- healthByEventType[healthByEventType$FATALITIES > 0, (c(1,2))]
injuries <- healthByEventType[healthByEventType$INJURIES > 0, (c(1,3))]

showTopInTable <- function(tb, colName) {
  meanVal <- mean(tb[[2]])
  a <- tb[tb[[2]] > meanVal,] # only show the top values
  a$pretty <- formatC(a[[2]], format="d", big.mark=",")
  names(a)[3] <- colName
  x <- a[order(-a[[2]]), c(1,3)] # sort descending by the 2nd column
  kable(x)
}

showTopInTable(fatalities, "fatalities")
showTopInTable(injuries, "injuries")

## Summarize Damage Data

# create method for formatting currency
printCurrency <- function(value, currency.sym="$", digits=0, sep=",", decimal=".") {
  paste(
    currency.sym,
    formatC(value, format = "f", big.mark = sep, digits=digits, decimal.mark=decimal),
    sep=""
  )
}

damageByEventType <- ddply(damage, .(eventType), numcolwise(sum))
damageByEventType$propCostInThousands <- printCurrency(damageByEventType$propDmgThousands)
damageByEventType$cropCostInThousands <- printCurrency(damageByEventType$cropDmgThousands)

# create function to display damage in a pretty table
showDamageTable <- function(dmgCol, formattedDmgCol, minDamage) {
  a <- damageByEventType[damageByEventType[[dmgCol]] > minDamage,]
  x <- a[order(-a[[dmgCol]]),c(1,formattedDmgCol)]
  kable(x)  # TODO: is xtable better?
}

# only show event types in the top quantile
q4PropDamage <- quantile(damageByEventType$propDmgThousands)[4]
q4CropDamage <- quantile(damageByEventType$cropDmgThousands)[4]

propDmgCol <- 6
cropDmgCol <- 7
propCostCol <- 8
cropCostCol <- 9

showDamageTable(propDmgCol, propCostCol, q4PropDamage)
showDamageTable(cropDmgCol, cropCostCol, q4CropDamage)


# Questions
# 
# Your data analysis must address the following questions:
#   
#   Across the United States, which types of events (as indicated in the EVTYPE variable) 
#   are most harmful with respect to population health?
#     FATALITIES          INJURIES
# 
# Across the United States, which types of events have the greatest economic consequences?
# 
# Consider writing your report as if it were to be read by a government or municipal manager 
# who might be responsible for preparing for severe weather events and will need to prioritize 
# resources for different types of events. However, there is no need to make any specific 
# recommendations in your report.


