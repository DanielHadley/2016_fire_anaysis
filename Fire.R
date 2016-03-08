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

shit_geo <- fd %>% group_by(Match_addr) %>% summarise(n=n()) %>% arrange(desc(n))
cat(paste(shQuote((shit_geo$Match_addr[1:20])), collapse=", "))

shit_geo2 <- fd %>% filter(Score < 95) %>% 
  group_by(Match_addr) %>% summarise(n=n()) %>% arrange(desc(n))
cat(paste(shQuote((shit_geo2$Match_addr[1:20])), collapse=", "))


bad_geocode <- c('268 Powder House Blvd, Somerville, Massachusetts, 02144', 'Somerville, Massachusetts', '408 Mystic Ave, Somerville, Massachusetts, 02145', 'NA', '75 Trull St, Somerville, Massachusetts, 02145', '130 Broadway, Somerville, Massachusetts, 02145', 'East Somerville, Massachusetts', 'shit geo 2 after this', '14 Kenneson Rd, Somerville, Massachusetts, 02145', '62 Marshall St, Somerville, Massachusetts, 02145', '1 Fitchburg St, Somerville, Massachusetts, 02143', '52 Florence Ter, Somerville, Massachusetts, 02145', '16 Chester Pl, Somerville, Massachusetts, 02144', 'Broadway & McGrath Hwy, Somerville, Massachusetts, 02145', '10 Dana St, Somerville, Massachusetts, 02145', '2 Franklin Ave, Somerville, Massachusetts, 02145', '93 North St, Somerville, Massachusetts, 02144', '93 South St, Somerville, Massachusetts, 02143', '52 White St Pl, Somerville, Massachusetts, 02140', 'Fellsway, Somerville, Massachusetts, 02145', '34 North St, Somerville, Massachusetts, 02144', '50 Memorial Rd, Somerville, Massachusetts, 02145', '56 Gilman Ter, Somerville, Massachusetts, 02145', '110 Thurston St, Somerville, Massachusetts, 02145', '33 Bonair St, Somerville, Massachusetts, 02145', '31 Prescott St, Somerville, Massachusetts, 02143', '511 Medford St, Somerville, Massachusetts, 02145')


fd_safer_geo <- fd %>% filter(Match_addr %notin% bad_geocode)

rm(shit_geo, shit_geo2)




#### Basic Analysis ####

# Let's try to recreate the calls per day in each district
# I'm Skeptical because many calls are from bad geocoded data
incidents_by_area <- fd_safer_geo %>%
  filter(Nature.of.Call != "TRAINING") %>% 
  # filter(Nature.of.Call %in% response_time_incidents) %>% 
  group_by(CAD.inc.Number, STATION) %>% 
  summarise(Year = Year) %>% 
  group_by(STATION, Year) %>% 
  summarise(n=n()) %>% 
  filter(!is.na(STATION)) %>% 
  spread(Year, n) %>% 
  mutate(Per.Change = (`2015` - `2009`) / `2009`) %>% 
  View()


# Avg calls, by day, by year, by area based on GIS analysis
# I'm Skeptical because many calls are from bad geocoded data
# A tweak in the geocoding could have major consequences for everything
avg_per_day_by_area <- fd_safer_geo %>%
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
  mutate(Per.Change = (`2015` - `2009`) / `2009`) %>% 
  View()


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
fd_to_map <- fd_safer_geo %>% filter(Nature.of.Call %in% response_time_incidents)
  

## Cluster analysis ##

# K means
# This is how we group incidents on a map.
# It may be more convenient to use reporting areas, but often those bisect a cluster
clust <- fd_to_map %>%
  dplyr::select(X, Y) %>%
  kmeans(5)


# Add cluster variable back to the data frame 
c <- augment(clust, fd_to_map) %>% select(.cluster)
fd_to_map$order <- seq(from = 1, to = nrow(fd_to_map))
c$order <- fd_to_map$order

fd_to_mapT <- merge(fd_to_map, c, by='order')

fd_to_map <- fd_to_mapT  %>% tbl_df() %>% mutate(cluster = .cluster) #the ."var name" throws off some functions

remove(c)

clusterCenters <- as.data.frame(clust$centers)


### Use maps to inspect the clusters ###

fd_to_map$X <- as.numeric(as.character(fd_to_map$X))
fd_to_map$Y <- as.numeric(as.character(fd_to_map$Y))


# Dot map 
map.center <- geocode("Somerville, MA")
SHmap <- qmap(c(lon=map.center$lon, lat=map.center$lat), source="google", zoom = 14, color='bw')
SHmap + geom_point(data=fd_to_map, aes(x=X, y=Y, color=fd_to_map$cluster))


# More traditional heat map
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


map.center <- geocode("Somerville, MA")
SHmap <- qmap(c(lon=map.center$lon, lat=map.center$lat), source="google", zoom = 13, color='bw')
SHmap + geom_point(data=clusterCenters, aes(x=X, y=Y), size = 15, alpha = 0.5, color = "Red")
