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


# I'm going to save a copy for Keith to geocode 
forKeith <- cad %>% group_by(Full.Address)  %>% summarise(n=n())
write.csv(forKeith, "./raw_data/forKeith.csv")


# PROBLEM : ambiguous addresses. Replace if possible
# inspect
byAddress <- cad  %>% group_by(Full.Address) %>% summarise(n=n())
cad$amb <- ifelse(cad$StNum == 0 | cad$Full.Address == "BROADWAY, Somerville, MA", 1, 0)
# Small enough problem I may not need to fix

# Not working TODO : fix
test <- merge(cad, geo, by.x = "Full.Address", by.y = "Full.Address", all.x = T)
notworking <- test %>% filter(is.na(X))
nwtable <- notworking  %>% group_by(Full.Address)  %>% summarise(n=n())




### Ok let's geocode the ones that are not working
d <- notworking %>% group_by(Full.Address)  %>% summarise(n=n()) %>% select(Full.Address)

# Geocodes using the Google engine
locs <- geocode(d$Full.Address)
#d <- bind_cols(d, locs) # Add the lat and long back to d
# ^ Didn't work, so
d$lon <- locs$lon
d$lat <- locs$lat

addToGeo <- d %>% mutate(Location = gsub(", Somerville, MA", "", Full.Address),
                         CountOfincnum = NA,
                         X = lon,
                         Y = lat,
                         Full.Address = Full.Address) %>% 
  select(-lon, -lat)

addToGeo <- addToGeo[c("Location","CountOfincnum","X","Y","Full.Address")]

geoFinal <- rbind(geo, addToGeo)

## Finally clean it up a bit
test <- d %>% mutate(total = paste(lon, lat)) %>% group_by(total) %>% summarise(n=n()) 

geoFinal <- geoFinal %>% filter(X != "-71.0994968",
                                 X != "-82.5346435",
                                 X != "-82.1660239",
                                 X != "-92.057063",
                                 X != "-119.6289038",
                                 X != "-80.8905922")

## Fuuuuuu didn't work too well. What's wrong with the Google geocode engine??
                                 

write.csv(geoFinal, "./data/police_fire_geoDB.csv")




######### Alternative: Use Keith's shapefile #########
# Maping tools
require("rgdal") # requires sp, will use proj.4 if installed
require("maptools")


mass <- readOGR(dsn = "./raw_data/dGeocodedResults", layer = "ResponseTimes_Geocoded")
mass@data$id = rownames(mass@data)
names(mass)
geo <- as.data.frame(mass@data)

write.csv(geo, "./raw_data/fire_geoDB")

# Now we will try to combine
# Load Data
geo <- read.csv("./raw_data/fire_geoDB")
cad <- read.csv("./data/CAD_table.csv")

geo <- geo %>% select(Location, X, Y, Incnum)
cad <- cad %>% select(IncNum, LocationGP, IncType)

test <- merge(cad, geo, by.x = "LocationGP", "Location", all.x = TRUE, all.y = FALSE)
blank <- test %>% filter(is.na(X))

## Ok, I see a major problem. 
# On all records where the address was a Blank & Blank, keith geocoded 0 Blank St