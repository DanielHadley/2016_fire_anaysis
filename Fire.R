# Created By Daniel Hadley Fri Feb 26 08:37:24 EST 2016 #
setwd("/Users/DHadley/Github/2016_fire_anaysis/")
setwd("/Users/dhadley/Documents/GitHub/2016_fire_anaysis/")

library(dplyr)
library(readxl)
library(lubridate)
library(tidyr)
library(broom) # augments d with model variables
library(ggplot2)
library(ggmap)


#### Load & Clean Data ####


# this is when it first gets entered into CAD
cad <- read.csv("./data/CAD_table.csv")

# This is the time it takes to get on location
# TODO: find out if the Unit associated with the first time is definitely the first responder
onloc <- read.csv("./data/ONLOC_times_TH_w_loc.csv")

# This is a record of basically whenever a Unit went out, so it's a good indicator of total activity
# Notice that several units will have the same Arrived time, 
# which is because they are all marked on the scene at the same time when the first arrives
# Note: this is marked as medical times in the CAD reporting system, but it contains all calls
ust <- read.csv("./data/unit_summary_times.csv")

# This is the database Chris created along with a thousand or so records I added
# I'm a little nervous about the ggmap geocoding engine - it did not seem to work well
# It may be worth going in and taking out strange addresses and large repeats of XY locations
geo <- read.csv("./data/police_fire_geoDB.csv")


## dates ##

# custom function for dealing with dates
# MyDataDate should be in quotes
DateVariableMaker <- function(MyData, MyDataDate){
  
  #Takes the date from data and adds important variables
  
  today <- Sys.Date()
  yesterday <- today - 1
  
  MyData$Date <- mdy_hm(MyData[,MyDataDate], tz = "EST")
  MyData$Year.Month <- format(MyData$Date, '%Y-%m')
  MyData$Month <- format(MyData$Date, '%m')
  MyData$Year <- format(MyData$Date, '%Y')
  MyData$DaysAgo <- difftime(MyData$Date, today, units = "days")
  
  return(MyData)
}

## ONLOC should only contain the first arrival ##
# Order by response time and then remove duplicates means you keep the first responder
onloc <- onloc[order(onloc$Incnum, onloc$Resp.Time),]
onloc <- onloc[!duplicated(onloc$Incnum),]

onloc <- DateVariableMaker(onloc, "Recd")


#### Basic Analysis ####

by_engine <- onloc %>% filter(Resp.Time < 10) %>% 
  filter(Unit == c("E3", "E1", "E2", "E3", "E6", "E7"))
rt <- ggplot(by_engine, aes(x=Resp.Time)) + geom_histogram(binwidth=.5,colour="white")
rt + facet_grid(Unit ~ .)

by_year <- onloc %>% filter(Resp.Time < 10) 
rt <- ggplot(by_year, aes(x=Resp.Time)) + geom_histogram(binwidth=.5,colour="white")
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
