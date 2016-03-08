# Created By Daniel Hadley Fri Feb 26 08:37:24 EST 2016 #
setwd("/Users/DHadley/Github/2016_fire_anaysis/")
setwd("/Users/dhadley/Documents/GitHub/2016_fire_anaysis/")

library(dplyr)
library(lubridate)
library(tidyr)
library(broom) # augments d with model variables
library(ggplot2)
library(ggmap)
library(RColorBrewer)




#### Load Data & Create One Comprehensive DB ####


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
# NOTE : I'm not using this because I'm nervous
# geo <- read.csv("./data/police_fire_geoDB.csv")

# This is the geo data produced by the google maps api
# First I cleaned up the addresses and then I wrote a special version of ggmaps
# which took my API key and batch geocoded them
# It's probably the best option
cad_geo <- read.csv("./data/CAD_table_geocoded_google.csv")


## Ok, now I want to make one large dataframe with every response ##
# I will use ust as the base and add other variables

# ONLOC should only contain the first arrival #
# Order by response time and then remove duplicates means you keep the first responder
onloc <- onloc[order(onloc$Incnum, onloc$Resp.Time),]
onloc <- onloc[!duplicated(onloc$Incnum),] 

onloc <- onloc %>% mutate(first.responder = Unit, first.responder.response.time = Resp.Time) %>% select(Incnum, first.responder, first.responder.response.time)

## Fire data!!
fd <- merge(ust, onloc, by.x = "CAD.inc.Number", by.y = "Incnum", all.x = TRUE)


## Ok now let's add in the Geo data
cad_geo <- cad_geo %>% select(- X, - n, - StName1: -IDtag)
fd <- merge(fd, cad_geo, by.x = "CAD.inc.Number", by.y = "IncNum", all.x = TRUE)


## Finally make a column for the first responder
first_responders <- onloc %>% mutate(first = paste(trimws(Incnum), trimws(first.responder))) %>% select(first)

first_responders <- as.vector(first_responders$first)

fd <- fd %>% mutate(first = paste(trimws(CAD.inc.Number), trimws(Unit))) 
fd$is.first.responder <- ifelse(fd$first %in% first_responders, 1, 0)
fd <- select(fd, -first)


write.csv(fd, "./data/Fire.csv", row.names = FALSE)




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


### Problems with Addresses ###

# TODO: hand fix a few of the worst ones
# Keep adding others
# IT seems like there are a lot of bad ones. W
# First an operator to negate in
`%notin%` <- Negate(`%in%`) 

# check_geo <- fd %>% group_by(Full.Address) %>% summarise(n=n()) %>% arrange(desc(n))
# cat(paste(shQuote((check_geo$Full.Address[1:80])), collapse=", "))

# TODO try to spot change Route 93 ones

# I just spot checked for the worst offenders
bad_geocode <- c('HYDRANT TESTING, Somerville, Massachusetts', 'HYDRANT SHOVELING, Somerville, Massachusetts', '1 MUTUAL AID - CAMBRIDGE, Somerville, Massachusetts', 'ROUTE 93 NORTH, Somerville, Massachusetts', 'ROUTE 93 SOUTH, Somerville, Massachusetts', '1 MUTUAL AID - EVERETT, Somerville, Massachusetts', 'HYDRANT TESTING, Somerville, Massachusetts', 'HYDRANT SHOVELING, Somerville, Massachusetts', '1 MUTUAL AID - CAMBRIDGE, Somerville, Massachusetts', '1 , Somerville, Massachusetts', '1 BIKE PATH, Somerville, Massachusetts')


fd$bad.geocode <- ifelse(is.na(fd$Full.Address), 1,
                         ifelse(fd$Full.Address %in% bad_geocode, 1,
                                ifelse(fd$lat < 42.37, 1,
                                       ifelse(fd$lon < 71.135, 0))))

# test <- fd %>% filter(bad.geocode == 1 & !is.na(Full.Address))




#### Basic Analysis ####


# Avg runs by day, by year, by unit
# This is higher than above because units go on many calls where they are not first responders
avg_per_day_by_unit <- fd %>%
  filter(Nature.of.Call != "TRAINING") %>% 
  # filter(Nature.of.Call %in% response_time_incidents) %>% 
  group_by(CAD.inc.Number, Unit, Date.Short) %>% 
  summarise(Year = Year) %>% 
  group_by(Unit, Year, Date.Short) %>% 
  summarise(n=n()) %>% 
  group_by(Unit, Year) %>% 
  summarise(m=mean(n)) %>% 
  filter(Unit != "") %>%
  data.frame() %>% 
  spread(Year, m) %>% 
  mutate(Per.Change = (`2015` - `2009`) / `2009`) %>% 
  View()


# Median response time, by day, by year, by unit (for first responders & important calls)
med_rt_by_unit <- fd %>%
  filter(Nature.of.Call %in% response_time_incidents & is.first.responder == 1) %>% 
  group_by(Unit, Year) %>% 
  summarise(m = median(first.responder.response.time)) %>% 
  filter(Unit != "") %>% 
  spread(Year, m) %>% 
  mutate(Per.Change = (`2015` - `2009`) / `2009`) %>% 
  View()


# Median response time, by day, by year, by unit (for first responders & important calls)
med_rt_by_unit_no_outliers <- fd %>%
  filter(Nature.of.Call %in% response_time_incidents &
           is.first.responder == 1 &
           first.responder.response.time < 10) %>% 
  group_by(Unit, Year) %>% 
  summarise(m = median(first.responder.response.time)) %>% 
  filter(Unit != "") %>% 
  spread(Year, m) %>% 
  mutate(Per.Change = (`2015` - `2009`) / `2009`) %>% 
  View()


# Median response time, by day, by year, by unit (for first responders & important calls)
med_rt_by_unit_engines <- fd %>%
  filter(Nature.of.Call %in% response_time_incidents &
           is.first.responder == 1 &
           Unit %in% engines) %>% 
  group_by(Unit, Year) %>% 
  summarise(m = median(first.responder.response.time)) %>% 
  filter(Unit != "") %>% 
  spread(Year, m) %>% 
  mutate(Per.Change = (`2015` - `2009`) / `2009`) %>% 
  View()


# ninetieth quantile response time, by day, by year, by unit (for first responders & important calls)
ninetieth_rt_by_unit <- fd %>%
  filter(Nature.of.Call %in% response_time_incidents & is.first.responder == 1) %>% 
  group_by(Unit, Year) %>% 
  summarise(nine = quantile(first.responder.response.time, .9)) %>% 
  filter(Unit != "") %>%
  data.frame() %>% 
  spread(Year, nine) %>% 
  mutate(Per.Change = (`2015` - `2009`) / `2009`) %>% 
  View()


fd_first <- fd %>% group_by(CAD.inc.Number, first.responder) %>% summarise()




## Histograms ##

# The following includes runs where the unit is first responder
by_engine <- fd %>% filter(first.responder.response.time < 10) %>%
  filter(Unit %in% engines &
           Nature.of.Call %in% response_time_incidents &
           is.first.responder == 1)
rt <- ggplot(by_engine, aes(x=first.responder.response.time)) + geom_histogram(binwidth=.5,colour="white")
rt + facet_grid(Unit ~ .)


by_year <- fd %>% filter(first.responder.response.time < 10 &
                           Nature.of.Call %in% response_time_incidents &
                           is.first.responder == 1) 
rt <- ggplot(by_year, aes(x=first.responder.response.time)) + geom_histogram(binwidth=.5,colour="white")
rt + facet_grid(Year ~ .)




#### Maps ####
# This includes all incidents, e.g., runs
fd_to_map <- fd %>% 
  filter(Nature.of.Call %in% response_time_incidents & bad.geocode == 0) %>%
  mutate(Y = lat, X = lon) %>% 
  filter(!is.na(X))



# Traditional heat map
Somerville = c(lon = -71.1000, lat =  42.3875)
somerville.map = get_map(location = Somerville, zoom = 14, maptype="roadmap",color = "bw")
ggmap(somerville.map, extent = "panel", maprange=FALSE) %+% fd_to_map + aes(x = fd_to_map$X, y = fd_to_map$Y) +
  # geom_density2d(data = d, aes(x = lon, y = lat)) + # uncomment for contour lines
  stat_density2d(data = fd_to_map, aes(x = X, y = Y,  fill = ..level.., alpha = ..level..),
                 size = 0.01, bins = 16, geom = 'polygon') +
  scale_fill_gradient(low = "yellow", high = "red") +
  scale_alpha(range = c(0.05, 0.75), guide = FALSE) +
  theme(legend.position = "none", axis.title = element_blank(), 
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        text = element_text(size = 12))
  




#### Cluster analysis ####

# K means
# This is how we group incidents on a map.
# It may be more convenient to use reporting areas, but often those bisect a cluster
# I'll do 15 b/c that will likely have enough geo detail
set.seed(123)

clust <- fd_to_map %>%
  select(X, Y) %>%
  kmeans(15)


# Add cluster variable back to the data frame 
c <- augment(clust, fd_to_map) %>% select(.cluster)
fd_to_map$order <- seq(from = 1, to = nrow(fd_to_map))
c$order <- fd_to_map$order

fd_to_mapT <- merge(fd_to_map, c, by='order')

fd_to_map <- fd_to_mapT  %>% tbl_df() %>% mutate(cluster = .cluster) #the ."var name" throws off some functions

remove(c, fd_to_mapT)

clusterCenters <- as.data.frame(clust$centers)


### Use maps to inspect the clusters ###

fd_to_map$X <- as.numeric(as.character(fd_to_map$X))
fd_to_map$Y <- as.numeric(as.character(fd_to_map$Y))


# Dot map 
map.center <- geocode("Somerville, MA")
SHmap <- qmap(c(lon=map.center$lon, lat=map.center$lat), source="google", zoom = 14, color='bw')
SHmap + geom_point(data=fd_to_map, aes(x=X, y=Y, color=fd_to_map$cluster))


# Cluster map
map.center <- geocode("Somerville, MA")
SHmap <- qmap(c(lon=map.center$lon, lat=map.center$lat), source="google", zoom = 13, color='bw')
SHmap + geom_point(data=clusterCenters, aes(x=X, y=Y), size = 15, alpha = 0.5, color = "Red")




### Analysis of clusters ###

# Let's try to recreate the calls per day in each cluster
incidents_by_area <- fd_to_map %>%
  group_by(CAD.inc.Number, cluster) %>% 
  summarise(Year = Year) %>% 
  group_by(cluster, Year) %>% 
  summarise(n=n()) %>%
  data.frame() %>%
  spread(Year, n) %>% 
  mutate(Per.Change = (`2015` - `2009`) / `2009`) 


# Now we will map this:
fd_to_map_incidents <- merge(fd_to_map, incidents_by_area, by.x = "cluster", by.y = "cluster")

# Dot map 
map.center <- geocode("Somerville, MA")
SHmap <- qmap(c(lon=map.center$lon, lat=map.center$lat), source="google", zoom = 13, color='bw')
SHmap + geom_point(data=fd_to_map_incidents, aes(x=X, y=Y, color=fd_to_map_incidents$Per.Change))



# Avg calls, by day, by year, by area based on GIS analysis
avg_per_day_by_area <- fd_to_map %>%
  group_by(CAD.inc.Number, cluster, Date.Short) %>% 
  summarise(Year = Year) %>% 
  group_by(cluster, Year, Date.Short) %>% 
  summarise(n=n()) %>% 
  group_by(cluster, Year) %>% 
  summarise(m=mean(n)) %>% 
  filter(!is.na(cluster)) %>% 
  data.frame() %>% 
  spread(Year, m) %>% 
  mutate(Per.Change = (`2015` - `2009`) / `2009`) 

# No map here b/c it is less critical


## The holy grail - response time changes ##
rt_by_area <- fd_to_map %>%
  filter(is.first.responder == 1) %>% 
  group_by(cluster, Year) %>%
  summarise(nine = quantile(first.responder.response.time, .9)) %>%
  data.frame() %>% 
  spread(Year, nine) %>% 
  mutate(Per.Change = (`2015` - `2009`) / `2009`)

# Now we will map this:
fd_to_map_rt <- merge(fd_to_map, rt_by_area, by.x = "cluster", by.y = "cluster")

fd_to_map_rt <- fd_to_map_rt %>% filter(is.first.responder == 1)

# Percent change 
map.center <- geocode("Somerville, MA")
SHmap <- qmap(c(lon=map.center$lon, lat=map.center$lat), source="google", zoom = 13, color='bw')
SHmap + geom_point(data=fd_to_map_rt, aes(x=X, y=Y, color=fd_to_map_rt$Per.Change))

# Dot map 2015 rt
map.center <- geocode("Somerville, MA")
SHmap <- qmap(c(lon=map.center$lon, lat=map.center$lat), source="google", zoom = 13, color='bw')
SHmap + geom_point(data=fd_to_map_rt, aes(x=X, y=Y, color=fd_to_map_rt$`2012`))




#### Better maps ####
theme_set(theme_bw(base_size = 8))

YlOrBr <- c("#FFFFD4", "#FED98E", "#FE9929", "#D95F0E", "#993404")

Somerville = c(lon = -71.1000, lat =  42.3875)
somerville.map = get_map(location = Somerville, zoom = 14, maptype="roadmap",color = "bw")

pred.stat.map <- ggmap(somerville.map) %+% fd_to_map + pred.stat
  aes(x = X,
      y = Y,
      z = first.responder.response.time) +
  stat_summary2d(fun = median, 
                 binwidth = c(.05, .05),
                 alpha = 0.5) + 
  scale_fill_gradientn(name = "Median",
                       colours = YlOrBr,
                       space = "Lab") + 
  labs(x = "Longitude",
       y = "Latitude") +
  coord_map()
print(pred.stat.map)


pred.stat <- ggplot(data = fd_to_map,
                    aes(x = X,
                        y = Y,
                        z = first.responder.response.time)) + 
                      stat_summary2d(fun = mean)
print(pred.stat)


# refine breaks and palette ----
require('RColorBrewer')
YlOrBr <- c("#FFFFD4", "#FED98E", "#FE9929", "#D95F0E", "#993404")
pred.stat.bin.width <- ggplot(data = fd_to_map,
                              aes(x = X,
                                  y = Y,
                                  z = first.responder.response.time)) + 
  stat_summary2d(fun = median, binwidth = c(.005, .005)) + 
  scale_fill_gradientn(name = "Median",
                       colours = YlOrBr,
                       space = "Lab") +
  coord_map()
print(pred.stat.bin.width)


map.in <- get_map(location = c(min(fd_to_map$X),
                               min(fd_to_map$Y),
                               max(fd_to_map$X),
                               max(fd_to_map$Y)),
                  source = "google")
theme_set(theme_bw(base_size = 8))

## working !!
Somerville = c(lon = -71.1000, lat =  42.3875)
map.in = get_map(location = Somerville, zoom = 13, maptype="roadmap",color = "bw")

pred.stat.map <- ggmap(map.in) %+% fd_to_map + 
  aes(x = X,
      y = Y,
      z = first.responder.response.time) +
  stat_summary2d(fun = median, 
                 binwidth = c(.001, .001),
                 alpha = 0.5) + 
  scale_fill_gradientn(name = "Median",
                       colours = YlOrBr,
                       space = "Lab") + 
  labs(x = "Longitude",
       y = "Latitude") +
  coord_map()
print(pred.stat.map)



## working !!
Somerville = c(lon = -71.1000, lat =  42.3875)
map.in = get_map(location = Somerville, zoom = 14, maptype="roadmap",color = "bw")

pred.stat.map <- ggmap(map.in) %+% fd_to_map + 
  aes(x = X,
      y = Y,
      z = first.responder.response.time) +
  stat_summary2d(fun = median, 
                 binwidth = c(.001, .001),
                 alpha = 0.5) + 
  scale_fill_gradientn(name = "Median",
                       colours = YlOrBr,
                       space = "Lab")
  coord_map()
print(pred.stat.map)



YlOrBr <- c("#FFFFD4", "#FED98E", "#FE9929", "#D95F0E", "#993404")
Somerville = c(lon = -71.1000, lat =  42.3875)
map.in = get_map(location = Somerville, zoom = 13, maptype="roadmap",color = "bw")

pred.stat.map <- ggmap(map.in) %+% fd_to_map + 
  aes(x = X,
      y = Y,
      z = first.responder.response.time) +
  stat_summary2d(fun = median, 
                 binwidth = c(.001, .001),
                 alpha = 0.5) + 
  scale_fill_gradientn(name = "Median",
                       colours = YlOrBr,
                       space = "Lab")
print(pred.stat.map)

ggsave("./plots/map.png", dpi=250, width=6, height=5)





YlOrBr <- c("#FFFFD4", "#FED98E", "#FE9929", "#D95F0E", "#993404")
Somerville = c(lon = -71.1000, lat =  42.3875)
map.in = get_map(location = Somerville, zoom = 13, maptype="roadmap",color = "bw")

pred.stat.map <- ggmap(map.in) %+% fd_to_map + 
  aes(x = X,
      y = Y,
      z = first.responder.response.time) +
  stat_summary2d(fun = median, 
                 binwidth = c(.001, .001),
                 alpha = 0.5) + 
  scale_fill_gradientn(name = "Median",
                       colours = YlOrBr,
                       space = "Lab")
print(pred.stat.map)

ggsave("./plots/map.png", dpi=250, width=6, height=5)