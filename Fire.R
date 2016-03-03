# Created By Daniel Hadley Fri Feb 26 08:37:24 EST 2016 #
setwd("/Users/DHadley/Github/2016_fire_anaysis/")
setwd("/Users/dhadley/Documents/GitHub/2016_fire_anaysis/")

library(dplyr)
library(lubridate)
library(tidyr)
library(broom) # augments d with model variables
library(ggplot2)
library(ggmap)


#### Load Data & Create One Comprehensive DB ####

# 
# # this is when it first gets entered into CAD
# cad <- read.csv("./data/CAD_table.csv")
# 
# # This is the time it takes to get on location
# # TODO: find out if the Unit associated with the first time is definitely the first responder
# onloc <- read.csv("./data/ONLOC_times_TH_w_loc.csv")
# 
# # This is a record of basically whenever a Unit went out, so it's a good indicator of total activity
# # Notice that several units will have the same Arrived time, 
# # which is because they are all marked on the scene at the same time when the first arrives
# # Note: this is marked as medical times in the CAD reporting system, but it contains all calls
# ust <- read.csv("./data/unit_summary_times.csv")
# 
# # This is the database Chris created along with a thousand or so records I added
# # I'm a little nervous about the ggmap geocoding engine - it did not seem to work well
# # It may be worth going in and taking out strange addresses and large repeats of XY locations
# # NOTE : I'm not using this because I'm nervous
# # geo <- read.csv("./data/police_fire_geoDB.csv")
# 
# # This is the geo data produced in collaboration with Keith
# # First I cleaned up the addresses and then I gave them to Keith to geocode
# # I avoided giving too many 0 Broadway, etc
# # It's probably the best option
# cad_geo <- read.csv("./data/CAD_table_geocoded.csv")
# 
# 
# ## Ok, now I want to make one large dataframe with every response ##
# # I will use ust as the base and add other variables
# 
# # ONLOC should only contain the first arrival #
# # Order by response time and then remove duplicates means you keep the first responder
# onloc <- onloc[order(onloc$Incnum, onloc$Resp.Time),]
# onloc <- onloc[!duplicated(onloc$Incnum),] 
# 
# onloc <- onloc %>% mutate(first.responder = Unit, first.responder.response.time = Resp.Time, is.first.responder = 1) %>% select(Incnum, first.responder, first.responder.response.time)
# 
# ## Fire data!!
# fd <- merge(ust, onloc, by.x = "CAD.inc.Number", by.y = "Incnum", all.x = TRUE)
# 
# 
# ## Ok now let's add in the Geo data
# cad_geo <- cad_geo %>% select(- X.1, - n, - LocForced, - StName1: -IDtag)
# fd <- merge(fd, cad_geo, by.x = "CAD.inc.Number", by.y = "IncNum", all.x = TRUE)
# 
# 
# write.csv(fd, "./data/Fire.csv", row.names = FALSE)




#### Load the Master database and Create variables ####
fd <- read.csv("./data/Fire.csv")

# Clean
fd <- fd %>% filter(CAD.inc.Number != "")

## dates ##
# custom function for dealing with dates
# MyDataDate should be in quotes
DateVariableMaker <- function(MyData, MyDataDate){
  
  #Takes the date from data and adds important variables
  
  today <- Sys.Date()
  yesterday <- today - 1
  
  MyData$Date <- mdy_hm(MyData[,MyDataDate], tz = "EST")
  MyData$Date.Short <- as.Date(MyData$Date)
  MyData$Year.Month <- format(MyData$Date, '%Y-%m')
  MyData$Month <- format(MyData$Date, '%m')
  MyData$Year <- format(MyData$Date, '%Y')
  # MyData$DaysAgo <- difftime(MyData$Date, today, units = "days")
  
  return(MyData)
}

fd <- DateVariableMaker(fd, "Date.Time.Received")


## Here is the list of important stuff to keep in your analysis
# by_nature <- fd %>% group_by(Nature.of.Call) %>% summarise(n=n())
# cat(paste(shQuote((shit$Nature.of.Call)), collapse=", "))
#
response_time_incidents <- c('A BOX ALARM', 'A FIRE ALARM', 'AUTO ACC INJURY', 'AUTO ACCIDENT', 'B BOX ALARM', 'B FIRE ALARM', 'BICYCLE ACC', 'BLDG COLLAPSE', 'BOMB REMOVAL', 'C BOX ALARM', 'C FIRE ALARM', 'CAR FIRE', 'CARB MON DETECT', 'CARB MON INCID', 'CONFINED SPACE', 'DUMPSTER FIRE', 'E BOX ALARM', 'E FIRE ALARM', 'ELEVATOR ACCID', 'ELEVATOR MEDIC', 'ELEVATOR RESCUE', 'FIRE', 'FIRE EXTINGUISH', 'GAS LEAK', 'GAS LEAK INSIDE', 'GAS LEAK OUTSID', 'HAZMAT INC A', 'HAZMAT INC L', 'HAZMAT INC R', 'HAZMAT INCIDENT', 'HOSTAGE ACTIVE', 'ICE RESCUE', 'INDUSTRIAL ACC', 'JUMPER', 'L BOX ALARM', 'L FIRE ALARM', 'LIFE LINE E', 'LOCK IN', 'MANHOLE FIRE', 'MEDICAL AID', 'MEDICAL AID E', 'MEDICAL AID L', 'MEDICAL AID R', 'OUTSIDE FIRE', 'PEDESTRIAN ACC', 'R BOX ALARM', 'R FIRE ALARM', 'S BOX ALARM', 'S FIRE ALARM', 'SHOOTING ACTIVE', 'STRIK BOX ALARM', 'STRIK FIRE ALAR', 'STRIKE BOX ALAR', 'STRK FIRE ALARM', 'TECH RESCUE', 'TRAIN INCIDENT', 'WATER RESCUE')


engines <- c("E3", "E1", "E2", "E3", "E6", "E7")




#### Basic Analysis ####

# Let's try to recreate the calls per day in each district
by_station_year <- fd %>%
  filter(Nature.of.Call != "TRAINING") %>% 
  # filter(Nature.of.Call %in% response_time_incidents) %>% 
  group_by(CAD.inc.Number, STATION) %>% 
  summarise(Year = Year) %>% 
  group_by(STATION, Year) %>% 
  summarise(n=n()) %>% 
  filter(!is.na(STATION)) %>% 
  spread(Year, n) %>% 
  mutate(Per.Change = (`2015` - `2009`) / `2009`)


# Avg calls, by day, by year, by station
avg_by_day_year_station <- fd %>%
  filter(Nature.of.Call != "TRAINING") %>% 
  # filter(Nature.of.Call %in% response_time_incidents) %>% 
  group_by(CAD.inc.Number, STATION, Date.Short) %>% 
  summarise(Year = Year) %>% 
  group_by(STATION, Year, Date.Short) %>% 
  summarise(n=n()) %>% 
  group_by(STATION, Year) %>% 
  summarise(m=mean(n)) %>% 
  filter(!is.na(STATION)) %>% 
  spread(Year, m) %>% 
  mutate(Per.Change = (`2015` - `2009`) / `2009`)





fd_first <- fd %>% group_by(CAD.inc.Number, first.responder) %>% summarise()

# The following includes all runs including ones where the engine is not a first responder
engines <- c("E3", "E1", "E2", "E3", "E6", "E7")
by_engine <- fd %>% filter(first.responder.response.time < 10) %>%
  filter(Unit %in% engines)
rt <- ggplot(by_engine, aes(x=first.responder.response.time)) + geom_histogram(binwidth=.5,colour="white")
rt + facet_grid(Unit ~ .)

by_year <- fd %>% filter(first.responder.response.time < 10) 
rt <- ggplot(by_year, aes(x=first.responder.response.time)) + geom_histogram(binwidth=.5,colour="white")
rt + facet_grid(Year ~ .)



## Make a proper Address Variable for CAD ##
cad$Full.Address <- ifelse(cad$StNum != "", paste(cad$StNum, cad$StName1), paste(cad$StName1, "&", cad$StName2))

cad$Full.Address <- ifelse(substr(cad$Full.Address, start = 1, stop = 3) == " & ", gsub(" & ", "", cad$Full.Address), cad$Full.Address)


## Take out unwanted from analysis
rtbl <- rtbl %>% filter(Inctype != c("TRAINING", "HYDRANT"))


## Dates ##
rtbl$Date <- as.Date(rtbl$Recd)


### Get geo coordinates
rtbl$Full.Address <- paste(rtbl$`Street Number`, " ", rtbl$Street, ", Somerville, MA", sep="")

# Clean to help with the merge
rtbl$Full.Address <- gsub("AV,", "AVE,", rtbl$Full.Address)
rtbl$Full.Address <- gsub(" A ", " ", rtbl$Full.Address)
rtbl$Full.Address <- gsub(" B ", " ", rtbl$Full.Address)

rtbl$Full.Address <- ifelse(substr(rtbl$Full.Address, start = 1, stop = 3) == "NA ", gsub("NA ", "", rtbl$Full.Address), rtbl$Full.Address)

rtbl$Full.Address <- ifelse(substr(rtbl$Full.Address, start = 1, stop = 2) == "0 ", gsub("0 ", "", rtbl$Full.Address), rtbl$Full.Address)

# Trim White space
rtbl$Full.Address <- trimws(rtbl$Full.Address)
geo$Full.Address <- trimws(geo$Full.Address)


# # Not working TODO fix later
# test <- merge(rtbl, geo, by.x = "Full.Address", by.y = "Full.Address", all.x = T)
# notworking <- test %>% filter(is.na(X))
# nwtable <- notworking  %>% group_by(Full.Address)  %>% summarise(n=n())


# Merge into the fully geocoded database
rtbl <- merge(rtbl, geo, by.x = "Full.Address", by.y = "Full.Address", all.x = T)
rtbl <- unique(rtbl)

rtblGeo <- filter(rtbl, X != "NA")




#### Basic Analysis ####

no_outliers <- rtblGeo %>% filter(`Resp Time` < 10) %>% 
  filter(Unit == c("E3", "E1", "E2", "E3", "E6", "E7"))

rt <- ggplot(no_outliers, aes(x=`Resp Time`)) + geom_histogram(binwidth=.5,colour="white")
rt + facet_grid(Unit ~ .)


### This does NOT seem to jive with Travis' numbers /??? ###

#### ???? Must be a function of the data I am using ####
by_day_and_engine <- no_outliers %>% group_by(Unit, Date) %>% summarise(n=n()) %>% group_by(Unit) %>% summarise(mean = mean(n))


# Weird on the zero response times
weird <- no_outliers %>% filter(`Resp Time` < .2)

## Let's see how long it takes E1 to respond to things in the East ##
# the X is about Somerville Ave and Washington St, Somerville, MA

E1 <- no_outliers %>% filter(Unit == "E1")
E3 <- no_outliers %>% filter(Unit == "E3")

E1_East <- rtblGeo %>% filter(Unit == "E1") %>% 
  mutate(X = as.numeric(levels(X))[X]) %>% 
  filter(X >= -71.0964)

E1_West <- rtblGeo %>% filter(Unit == "E1") %>% 
  mutate(X = as.numeric(levels(X))[X]) %>% 
  filter(X < -71.0964)

hist(E1_West$`Resp Time`[E1_West$`Resp Time` < 10])
hist(E1_East$`Resp Time`[E1_East$`Resp Time` < 10])


E3 <- rtblGeo %>% filter(Unit == "E3")

E3_East <- rtblGeo %>% filter(Unit == "E3") %>% 
  mutate(X = as.numeric(levels(X))[X]) %>% 
  filter(X >= -71.0964)

E3_West <- rtblGeo %>% filter(Unit == "E3") %>% 
  mutate(X = as.numeric(levels(X))[X]) %>% 
  filter(X < -71.0964)

hist(E3_West$`Resp Time`[E3_West$`Resp Time` < 10])
hist(E3_East$`Resp Time`[E3_East$`Resp Time` < 10])




#### Cluster analysis ####

# K means
# This is how we group incidents on a map.
# It may be more convenient to use reporting areas, but often those bisect a cluster
clust <- rtblGeo %>%
  dplyr::select(X, Y) %>%
  kmeans(5)


# Add cluster variable back to the data frame 
c <- augment(clust, rtblGeo) %>% select(.cluster)
rtblGeo$order <- seq(from = 1, to = nrow(rtblGeo))
c$order <- rtblGeo$order

rtblGeoT <- merge(rtblGeo, c, by='order')

rtblGeo <- rtblGeoT  %>% tbl_df() %>% mutate(cluster = .cluster) #the ."var name" throws off some functions

remove(c)

clusterCenters <- as.data.frame(clust$centers)


### Use maps to inspect the clusters ###

rtblGeo$X <- as.numeric(as.character(rtblGeo$X))
rtblGeo$Y <- as.numeric(as.character(rtblGeo$Y))


# Dot map 
map.center <- geocode("Somerville, MA")
SHmap <- qmap(c(lon=map.center$lon, lat=map.center$lat), source="google", zoom = 14, color='bw')
SHmap + geom_point(data=rtblGeo, aes(x=X, y=Y, color=rtblGeo$cluster))


# More traditional heat map
Somerville = c(lon = -71.1000, lat =  42.3875)
somerville.map = get_map(location = Somerville, zoom = 14, maptype="roadmap",color = "bw")
ggmap(somerville.map, extent = "panel", maprange=FALSE) %+% rtblGeo + aes(x = rtblGeo$X, y = rtblGeo$Y) +
  # geom_density2d(data = d, aes(x = lon, y = lat)) + # uncomment for contour lines
  stat_density2d(data = rtblGeo, aes(x = X, y = Y,  fill = ..level.., alpha = ..level..),
                 size = 0.01, bins = 16, geom = 'polygon') +
  scale_fill_gradient(low = "yellow", high = "red") +
  scale_alpha(range = c(0.05, 0.75), guide = FALSE) +
  theme(legend.position = "none", axis.title = element_blank(), 
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        text = element_text(size = 12))


map.center <- geocode("Somerville, MA")
SHmap <- qmap(c(lon=map.center$lon, lat=map.center$lat), source="google", zoom = 13, color='bw')
SHmap + geom_point(data=clusterCenters, aes(x=X, y=Y), size = 15, alpha = 0.5, color = "Red")




#### Cluster analysis of Just E1 & E3 ####

# K means
# This is how we group incidents on a map.
# It may be more convenient to use reporting areas, but often those bisect a cluster
rtblGeo <-  filter(rtblGeo, Unit == "E3" | Unit == "E1")

clust <- rtblGeo %>%
  dplyr::select(X, Y) %>%
  kmeans(2)


# Add cluster variable back to the data frame 
c <- augment(clust, rtblGeo) %>% select(.cluster)
rtblGeo$order <- seq(from = 1, to = nrow(rtblGeo))
c$order <- rtblGeo$order

rtblGeoT <- merge(rtblGeo, c, by='order')

rtblGeo <- rtblGeoT  %>% tbl_df() %>% mutate(cluster = .cluster) #the ."var name" throws off some functions

remove(c)

clusterCenters <- as.data.frame(clust$centers)


### Use maps to inspect the clusters ###

rtblGeo$X <- as.numeric(as.character(rtblGeo$X))
rtblGeo$Y <- as.numeric(as.character(rtblGeo$Y))


# Dot map 
map.center <- geocode("Union Square, Somerville, MA")
SHmap <- qmap(c(lon=map.center$lon, lat=map.center$lat), source="google", zoom = 14, color='bw')
SHmap + geom_point(data=rtblGeo, aes(x=X, y=Y), color=rtblGeo$cluster, alpha = .03, size = 3)


map.center <- geocode("Union Square, Somerville, MA")
SHmap <- qmap(c(lon=map.center$lon, lat=map.center$lat), source="google", zoom = 15, color='bw')
SHmap + geom_point(data=clusterCenters, aes(x=X, y=Y), size = 15, alpha = 0.5, color = "Red")




#### Cluster analysis of Just E3 ####

# K means
# This is how we group incidents on a map.
# It may be more convenient to use reporting areas, but often those bisect a cluster
rtblGeo <-  filter(rtblGeo, Unit == "E3")

clust <- rtblGeo %>%
  dplyr::select(X, Y) %>%
  kmeans(1)


# Add cluster variable back to the data frame 
c <- augment(clust, rtblGeo) %>% select(.cluster)
rtblGeo$order <- seq(from = 1, to = nrow(rtblGeo))
c$order <- rtblGeo$order

rtblGeoT <- merge(rtblGeo, c, by='order')

rtblGeo <- rtblGeoT  %>% tbl_df() %>% mutate(cluster = .cluster) #the ."var name" throws off some functions

remove(c)

clusterCenters <- as.data.frame(clust$centers)


### Use maps to inspect the clusters ###

rtblGeo$X <- as.numeric(as.character(rtblGeo$X))
rtblGeo$Y <- as.numeric(as.character(rtblGeo$Y))


# Dot map 
map.center <- geocode("Union Square, Somerville, MA")
SHmap <- qmap(c(lon=map.center$lon, lat=map.center$lat), source="google", zoom = 15, color='bw')
SHmap + geom_point(data=rtblGeo, aes(x=X, y=Y), color="Red", alpha = .03, size = 4)


map.center <- geocode("Union Square, Somerville, MA")
SHmap <- qmap(c(lon=map.center$lon, lat=map.center$lat), source="google", zoom = 15, color='bw')
SHmap + geom_point(data=clusterCenters, aes(x=X, y=Y), size = 15, alpha = 0.5, color = "Red")


# More traditional heat map
Somerville = c(lon = -71.0956, lat =  42.37965)
somerville.map = get_map(location = Somerville, zoom = 16, maptype="roadmap",color = "bw")
ggmap(somerville.map, extent = "panel", maprange=FALSE) %+% rtblGeo + aes(x = rtblGeo$X, y = rtblGeo$Y) +
  # geom_density2d(data = d, aes(x = lon, y = lat)) + # uncomment for contour lines
  stat_density2d(data = rtblGeo, aes(x = X, y = Y,  fill = ..level.., alpha = ..level..),
                 size = 0.01, bins = 16, geom = 'polygon') +
  scale_fill_gradient(low = "green", high = "red") +
  scale_alpha(range = c(0.00, 0.25), guide = FALSE) +
  theme(legend.position = "none", axis.title = element_blank(), 
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        text = element_text(size = 12))
