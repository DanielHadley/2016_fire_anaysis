# Created By Daniel Hadley Wed Mar 2 15:24:16 EST 2016 #
# To geocode the CAD calls
setwd("/Users/DHadley/Github/2016_fire_anaysis")
setwd("/Users/dhadley/Documents/GitHub/2016_fire_anaysis/")

library(dplyr)
library(ggmap)

# Load Data
cad <- read.csv("./data/CAD_table.csv")

# This is the geo database that Chris and I built geocoding all Police Calls
geo <- read.csv("./raw_data/LibCoordinates.csv")


# Strip cad to the essentials
cad <- cad %>% select(IncNum, IncType, LocType:NearTag)

## Make a proper Address Variable for CAD ##
cad$Address <- ifelse(cad$StNum != "", paste(cad$StNum, cad$StName1), paste(cad$StName1, "&", cad$StName2))

cad$Address <- ifelse(substr(cad$Address, start = 1, stop = 3) == " & ", gsub(" & ", "", cad$Address), cad$Address)

# Take out blanks because the vast majority are out of town or training
cad <- cad %>% filter(Address != "")

cad$Full.Address <- paste(cad$Address, ", Somerville, MA", sep="")

# Trim White space
cad$Full.Address <- trimws(cad$Full.Address)
geo$Full.Address <- trimws(geo$Full.Address)


## Clean to help with the merge
cad$Full.Address <- gsub("AV,", "AVE,", cad$Full.Address)
cad$Full.Address <- gsub("AV &", "AVE &", cad$Full.Address)
cad$Full.Address <- gsub(" A ", " ", cad$Full.Address)
cad$Full.Address <- gsub(" B ", " ", cad$Full.Address)
cad$Full.Address <- gsub(" & ,", ",", cad$Full.Address)
cad$Full.Address <- gsub(" OPPOSITE ", " ", cad$Full.Address)

cad$Full.Address <- ifelse(substr(cad$Full.Address, start = 1, stop = 3) == "NA ", gsub("NA ", "", cad$Full.Address), cad$Full.Address)

cad$Full.Address <- ifelse(substr(cad$Full.Address, start = 1, stop = 2) == "0 ", gsub("0 ", "", cad$Full.Address), cad$Full.Address)

# Not working TODO : fix
test <- merge(cad, geo, by.x = "Full.Address", by.y = "Full.Address", all.x = T)
notworking <- test %>% filter(is.na(X))
nwtable <- notworking  %>% group_by(Full.Address)  %>% summarise(n=n())

# PROBLEM : ambiguous addresses. TODO: maybe fix
amb <- cad %>% filter(StNum == 0)


# Merge into the fully geocoded database
cad <- merge(cad, geo, by.x = "Full.Address", by.y = "Full.Address", all.x = T)
cad <- unique(cad)

cadGeo <- filter(cad, X != "NA")

