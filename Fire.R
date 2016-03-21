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

# # I did this and saved the result - no need for this code now
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
# # This is the geo data produced by the google maps api
# # First I cleaned up the addresses and then I wrote a special version of ggmaps
# # which took my API key and batch geocoded them
# # It's probably the best option
# cad_geo <- read.csv("./data/CAD_table_geocoded_google.csv")
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
# onloc <- onloc %>% mutate(first.responder = Unit, first.responder.response.time = Resp.Time) %>% select(Incnum, first.responder, first.responder.response.time)
# 
# ## Fire data!!
# fd <- merge(ust, onloc, by.x = "CAD.inc.Number", by.y = "Incnum", all.x = TRUE)
# 
# 
# ## Ok now let's add in the Geo data
# cad_geo <- cad_geo %>% select(- X, - n, - StName1: -IDtag)
# fd <- merge(fd, cad_geo, by.x = "CAD.inc.Number", by.y = "IncNum", all.x = TRUE)
# 
# 
# ## Finally make a column for the first responder
# first_responders <- onloc %>% mutate(first = paste(trimws(Incnum), trimws(first.responder))) %>% select(first)
# 
# first_responders <- as.vector(first_responders$first)
# 
# fd <- fd %>% mutate(first = paste(trimws(CAD.inc.Number), trimws(Unit))) 
# fd$is.first.responder <- ifelse(fd$first %in% first_responders, 1, 0)
# fd <- select(fd, -first)
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

# and just because I get confused by lat and lon
fd <- fd %>% mutate(Y = lat, X = lon)




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
  mutate(Per.Change.2009.to.2015 = (`2015` - `2009`) / `2009`) %>%
  arrange(desc(`2015`)) %>% 
  write.csv("./plots/avg_per_day_by_unit.csv")
  View() %>% 



# Median response time, by day, by year, by unit (for first responders & important calls)
med_rt_by_unit <- fd %>%
  filter(Nature.of.Call %in% response_time_incidents & is.first.responder == 1) %>% 
  group_by(Unit, Year) %>% 
  summarise(m = median(first.responder.response.time)) %>% 
  filter(Unit != "") %>% 
  spread(Year, m) %>% 
  mutate(Per.Change.2009.to.2015 = (`2015` - `2009`) / `2009`) %>% 
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
  mutate(Per.Change.2009.to.2015 = (`2015` - `2009`) / `2009`) %>% 
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
  mutate(Per.Change.2009.to.2015 = (`2015` - `2009`) / `2009`) %>% 
  View()


# ninetieth quantile response time, by day, by year, by unit (for first responders & important calls)
ninetieth_rt_by_unit <- fd %>%
  filter(Nature.of.Call %in% response_time_incidents & is.first.responder == 1) %>% 
  group_by(Unit, Year) %>% 
  summarise(nine = quantile(first.responder.response.time, .9)) %>% 
  filter(Unit != "") %>%
  data.frame() %>% 
  spread(Year, nine) %>% 
  mutate(Per.Change.2009.to.2015 = (`2015` - `2009`) / `2009`) %>%
  write.csv("./plots/ninetieth_quantile_response_times_by_unit.csv")
  View()





## Histograms ##

# The following includes runs where the unit is first responder
by_engine <- fd %>% filter(first.responder.response.time < 7.5) %>%
  filter(Unit %in% engines &
           Nature.of.Call %in% response_time_incidents &
           is.first.responder == 1)

rt <- ggplot(by_engine, aes(x=first.responder.response.time)) + 
  geom_histogram(binwidth=.5,colour="white") +
  labs(title = "Response Times By Unit", x = "Minutes") +
  facet_grid(Unit ~ .)

ggsave("./plots/response_times_by_engine.png", dpi=250, width=6, height=5)



by_year <- fd %>% filter(first.responder.response.time < 7.5 &
                           Nature.of.Call %in% response_time_incidents &
                           is.first.responder == 1) 
rt <- ggplot(by_year, aes(x=first.responder.response.time)) + 
  geom_histogram(binwidth=.5,colour="white") +
  labs(title = "Response Times By Year", x = "Minutes") +
  facet_grid(Year ~ .)

ggsave("./plots/response_times_by_year.png", dpi=250, width=6, height=5)




#### Maps ####



# All Runs For Critical Incidents
# This includes all incidents, e.g., runs
fd_to_map <- fd %>% 
  filter(Nature.of.Call %in% response_time_incidents & bad.geocode == 0) %>%
  mutate(Y = lat, X = lon) %>% 
  filter(!is.na(X))

Somerville = c(lon = -71.1000, lat =  42.3875)
somerville.map = get_map(location = Somerville, zoom = 13, maptype="roadmap",color = "bw")
ggmap(somerville.map, extent = "panel", maprange=FALSE) %+% fd_to_map + aes(x = fd_to_map$X, y = fd_to_map$Y) +
  # geom_density2d(data = d, aes(x = lon, y = lat)) + # uncomment for contour lines
  stat_density2d(data = fd_to_map, aes(x = X, y = Y,  fill = ..level.., alpha = ..level..),
                 size = 0.01, bins = 16, geom = 'polygon') +
  scale_fill_gradient(low = "yellow", high = "red") +
  scale_alpha(range = c(0.05, 0.75), guide = FALSE) +
  theme(legend.position = "none", axis.title = element_blank(), 
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        text = element_text(size = 12)) +
  ggtitle("Runs For Critical Incidents: 2009-2015")

ggsave("./plots/All_Runs_For_Critical_Incidents.png", dpi=250, width=6, height=5)
  



# All calls For Critical Incidents
# This includes all incidents, e.g., runs
fd_to_map <- fd %>% 
  filter(Nature.of.Call %in% response_time_incidents & bad.geocode == 0) %>%
  group_by(CAD.inc.Number) %>% 
  summarise(X = min(X), Y = min(Y)) %>% 
  filter(!is.na(X))

Somerville = c(lon = -71.1000, lat =  42.3875)
somerville.map = get_map(location = Somerville, zoom = 13, maptype="roadmap",color = "bw")
ggmap(somerville.map, extent = "panel", maprange=FALSE) %+% fd_to_map + aes(x = fd_to_map$X, y = fd_to_map$Y) +
  # geom_density2d(data = d, aes(x = lon, y = lat)) + # uncomment for contour lines
  stat_density2d(data = fd_to_map, aes(x = X, y = Y,  fill = ..level.., alpha = ..level..),
                 size = 0.01, bins = 16, geom = 'polygon') +
  scale_fill_gradient(low = "yellow", high = "red") +
  scale_alpha(range = c(0.05, 0.75), guide = FALSE) +
  theme(legend.position = "none", axis.title = element_blank(), 
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        text = element_text(size = 12)) +
  ggtitle("Calls For Critical Incidents: 2009-2015")

ggsave("./plots/All_Calls_For_Critical_Incidents.png", dpi=250, width=6, height=5)




# All calls For Critical Incidents
# This includes all incidents, e.g., runs
fd_to_map <- fd %>% 
  filter(Nature.of.Call %in% response_time_incidents &
           bad.geocode == 0 &
           Year != 2009) %>%
  group_by(CAD.inc.Number) %>% 
  summarise(X = min(X), Y = min(Y), Year = Year) %>% 
  filter(!is.na(X))

Somerville = c(lon = -71.1000, lat =  42.3875)
somerville.map = get_map(location = Somerville, zoom = 13, maptype="roadmap",color = "bw")
ggmap(somerville.map, extent = "panel", maprange=FALSE) %+% fd_to_map + aes(x = fd_to_map$X, y = fd_to_map$Y) +
  # geom_density2d(data = d, aes(x = lon, y = lat)) + # uncomment for contour lines
  stat_density2d(data = fd_to_map, aes(x = X, y = Y,  fill = ..level.., alpha = ..level..),
                 size = 0.01, bins = 16, geom = 'polygon') +
  scale_fill_gradient(low = "yellow", high = "red") +
  scale_alpha(range = c(0.05, 0.75), guide = FALSE) +
  theme(legend.position = "none", axis.title = element_blank(), 
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        text = element_text(size = 12)) +
  ggtitle("Calls For Critical Incidents") +
  facet_wrap(~ Year)

ggsave("./plots/All_Calls_For_Critical_Incidents_year.png", dpi=250, width=6, height=5)




# All RTs over 5 for first responders facet
# This includes all incidents, e.g., runs
fd_to_map <- fd %>% 
  filter(Nature.of.Call %in% response_time_incidents &
           bad.geocode == 0 &
           Year != 2009 &
           is.first.responder == 1 &
           first.responder.response.time > 5) %>%
  group_by(CAD.inc.Number) %>% 
  summarise(X = min(X), Y = min(Y), Year = Year) %>% 
  filter(!is.na(X))

Somerville = c(lon = -71.1000, lat =  42.3875)
somerville.map = get_map(location = Somerville, zoom = 13, maptype="roadmap",color = "bw")
ggmap(somerville.map, extent = "panel", maprange=FALSE) %+% fd_to_map + aes(x = fd_to_map$X, y = fd_to_map$Y) +
  # geom_density2d(data = d, aes(x = lon, y = lat)) + # uncomment for contour lines
  stat_density2d(data = fd_to_map, aes(x = X, y = Y,  fill = ..level.., alpha = ..level..),
                 size = 0.01, bins = 16, geom = 'polygon') +
  scale_fill_gradient(low = "yellow", high = "red") +
  scale_alpha(range = c(0.05, 0.75), guide = FALSE) +
  theme(legend.position = "none", axis.title = element_blank(), 
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        text = element_text(size = 12)) +
  ggtitle("Calls with Response Times Over 5 Minutes") +
  facet_wrap(~ Year)

ggsave("./plots/Over_Five_For_Critical_Incidents_year.png", dpi=250, width=6, height=5)




# All RTs over 5 for first responders 
# This includes all incidents, e.g., runs
fd_to_map <- fd %>% 
  filter(Nature.of.Call %in% response_time_incidents &
           bad.geocode == 0 &
           is.first.responder == 1 &
           first.responder.response.time > 5) %>%
  group_by(CAD.inc.Number) %>% 
  summarise(X = min(X), Y = min(Y), Year = Year) %>% 
  filter(!is.na(X))

Somerville = c(lon = -71.1000, lat =  42.3875)
somerville.map = get_map(location = Somerville, zoom = 13, maptype="roadmap",color = "bw")
ggmap(somerville.map, extent = "panel", maprange=FALSE) %+% fd_to_map + aes(x = fd_to_map$X, y = fd_to_map$Y) +
  # geom_density2d(data = d, aes(x = lon, y = lat)) + # uncomment for contour lines
  stat_density2d(data = fd_to_map, aes(x = X, y = Y,  fill = ..level.., alpha = ..level..),
                 size = 0.01, bins = 16, geom = 'polygon') +
  scale_fill_gradient(low = "yellow", high = "red") +
  scale_alpha(range = c(0.05, 0.75), guide = FALSE) +
  theme(legend.position = "none", axis.title = element_blank(), 
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        text = element_text(size = 12)) +
  ggtitle("Calls with Response Times Over 5 Minutes: 2009-2015")

ggsave("./plots/Over_Five_For_Critical_Incidents.png", dpi=250, width=6, height=5)




# The holy grail: map of response times
rt_first_responders <- fd %>% 
  filter(Nature.of.Call %in% response_time_incidents &
           bad.geocode == 0 &
           is.first.responder == 1 &
           !is.na(X))

Somerville = c(lon = -71.1000, lat =  42.3875)
map.in = get_map(location = Somerville, zoom = 13, maptype="roadmap",color = "bw")

fire.map <- ggmap(map.in) %+% rt_first_responders + 
  aes(x = X,
      y = Y,
      z = first.responder.response.time) +
  stat_summary2d(fun = median, 
                 binwidth = c(.002, .002),
                 alpha = 0.5) + 
  scale_fill_gradientn(name = "minutes", colours=(brewer.pal(9,"YlGnBu"))) +
  labs(fill="") +
  theme_nothing(legend=TRUE) + ggtitle("Median Response Times: 2009-2015")
print(fire.map)

ggsave("./plots/median_response_times_map.png", dpi=250, width=6, height=5)




# The holy grail: map of response times in 90th
rt_first_responders <- fd %>% 
  filter(Nature.of.Call %in% response_time_incidents &
           bad.geocode == 0 &
           is.first.responder == 1 &
           !is.na(X))

Somerville = c(lon = -71.1000, lat =  42.3875)
map.in = get_map(location = Somerville, zoom = 13, maptype="roadmap",color = "bw")

fire.map <- ggmap(map.in) %+% rt_first_responders + 
  aes(x = X,
      y = Y,
      z = first.responder.response.time) +
  stat_summary2d(fun.y = "quantile", fun.args=list(probs=0.9), 
                 binwidth = c(.002, .002),
                 alpha = 0.5) + 
  scale_fill_gradientn(name = "minutes", colours=(brewer.pal(9,"YlGnBu"))) +
  labs(fill="") +
  theme_nothing(legend=TRUE) + ggtitle("90th Percentile Response Times: 2009-2015")
print(fire.map)

ggsave("./plots/ninetieth_per_response_times.png", dpi=250, width=6, height=5)




# The holy grail: map of response times in 90th
#! Important: I set the limits to the scale : ones above that are grey
rt_first_responders <- fd %>% 
  filter(Nature.of.Call %in% response_time_incidents &
           bad.geocode == 0 &
           is.first.responder == 1 &
           Year != 2009 &
           !is.na(X))

Somerville = c(lon = -71.1000, lat =  42.3875)
map.in = get_map(location = Somerville, zoom = 13, maptype="roadmap",color = "bw")

fire.map <- ggmap(map.in) %+% rt_first_responders + 
  aes(x = X,
      y = Y,
      z = first.responder.response.time) +
  stat_summary2d(fun.y = "quantile", fun.args=list(probs=0.9), 
                 binwidth = c(.002, .002),
                 alpha = 0.5) + 
  scale_fill_gradientn(name = "minutes", colours=(brewer.pal(9,"YlGnBu")), limits = c(1,7)) +
  labs(fill="") +
  theme_nothing(legend=TRUE) + ggtitle(expression(atop("90th Percentile Response Times", atop(italic("Scale capped at 7"), "")))) +
  facet_wrap(~ Year)
  

ggsave("./plots/ninetieth_per_response_times_Year.png", dpi=250, width=6, height=5)






## Dot maps ##

# A for loop that will create a dot map for every neighborhood you specify
neighborhoodList <- c("Assembly Square", "Ball Square", "Davis Square", "East Somerville", "Gilman Square", "Magoun Square", "Porter Square", "Prospect Hill", "Spring Hill", "Teele Square", "Ten Hills", "Union Square", "Winter Hill", "Inner Belt", "Ward Two")

fd_dot_map <- fd %>% 
  filter(Nature.of.Call %in% response_time_incidents &
           bad.geocode == 0 &
           is.first.responder == 1 &
           !is.na(X) &
           Year == 2015) %>%
  mutate(XY = paste(X, Y)) %>% 
  group_by(XY) %>% 
  summarise(n = n(), X = min(X), Y = min(Y), rt = median(first.responder.response.time))

for (n in 1:(length(neighborhoodList))) {
  map <- get_map(location = paste(neighborhoodList[n], "Somerville, MA", sep=", "), zoom=16, maptype="roadmap", color = "bw")
  ggmap(map) + 
    geom_point(data = fd_dot_map,
               aes(x = X, y = Y, colour = rt, size = n)) +
    scale_colour_gradientn(name = "minutes", colours=(brewer.pal(9,"YlGnBu")), limits = c(1,7)) +
    scale_size(name = "calls", range=c(1,20)) +
    labs(fill="") + 
    theme_nothing(legend=TRUE) +
    ggtitle(paste("2015 Calls: ",neighborhoodList[n]))
  
  ggsave(paste("./plots/map_",neighborhoodList[n], ".png", sep=""), dpi=250, width=6, height=5)
}



## Over 5 minutes
# A for loop that will create a dot map for every neighborhood you specify
neighborhoodList <- c("Assembly Square", "Ball Square", "Davis Square", "East Somerville", "Gilman Square", "Magoun Square", "Porter Square", "Prospect Hill", "Spring Hill", "Teele Square", "Ten Hills", "Union Square", "Winter Hill", "Inner Belt", "Ward Two")

fd_dot_map_over_5 <- fd %>% 
  filter(Nature.of.Call %in% response_time_incidents &
           bad.geocode == 0 &
           is.first.responder == 1 &
           !is.na(X) &
           first.responder.response.time > 5) %>%
  mutate(XY = paste(X, Y)) 

for (n in 1:(length(neighborhoodList))) {
  map <- get_map(location = paste(neighborhoodList[n], "Somerville, MA", sep=", "), zoom=16, maptype="roadmap", color = "bw")
  ggmap(map) + 
    geom_point(data = fd_dot_map_over_5,
               aes(x = X, y = Y, size = 10, alpha = .7, bins = 26, color="red")) +
    labs(x="",y="") +
    theme(legend.position = "none", axis.title = element_blank(), 
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          text = element_text(size = 12)) +
    ggtitle(paste("Calls Over 5 Minutes: ",neighborhoodList[n]))
  
  ggsave(paste("./plots/map_",neighborhoodList[n], "_over_5.png", sep=""), dpi=250, width=6, height=5)
}



## Over 5 minutes by Year
# A for loop that will create a dot map for every neighborhood you specify
neighborhoodList <- c("Assembly Square", "Ball Square", "Davis Square", "East Somerville", "Gilman Square", "Magoun Square", "Porter Square", "Prospect Hill", "Spring Hill", "Teele Square", "Ten Hills", "Union Square", "Winter Hill", "Inner Belt", "Ward Two")

fd_dot_map_over_5 <- fd %>% 
  filter(Nature.of.Call %in% response_time_incidents &
           bad.geocode == 0 &
           is.first.responder == 1 &
           !is.na(X) &
           first.responder.response.time > 5 &
           Year != 2009) %>%
  mutate(XY = paste(X, Y)) 

for (n in 1:(length(neighborhoodList))) {
  map <- get_map(location = paste(neighborhoodList[n], "Somerville, MA", sep=", "), zoom=16, maptype="roadmap", color = "bw")
  ggmap(map) + 
    geom_point(data = fd_dot_map_over_5,
               aes(x = X, y = Y, size = 10, alpha = .7, bins = 26, color="red")) +
    labs(x="",y="") +
    theme(legend.position = "none", axis.title = element_blank(), 
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          text = element_text(size = 12)) +
    ggtitle(paste("Calls Over 5 Minutes: ",neighborhoodList[n])) +
    facet_wrap(~ Year)
  
  ggsave(paste("./plots/map_",neighborhoodList[n], "_over_5_by_Year.png", sep=""), dpi=250, width=6, height=5)
}




#### Geo Analysis ####
## This should hopefully get us a simple model of resonse times from various locations

# ### Make the data ####
# ## First 515 Somerville
# # We will filter it down to E1 and E3 calls, which are mostly south east
# # We don't just do ones where they are first responder
# set.seed(124)
# 
# DF <- fd %>% 
#   filter(Unit == "E1" | Unit == "E3" &
#            is.na(X) == FALSE) %>% 
#   sample_n(1000) %>% 
#   mutate(to = paste(Y, X, sep = ", ")) %>% 
#   select(to, Full.Address) %>% 
#   group_by(Full.Address) %>% 
#   summarise(to = min(to), n = n()) %>% 
#   mutate(from = "515 Somerville Ave, Somerville, MA",
#          minutes = "",
#          seconds = "",
#          miles = "")
# 
# DF$row.number <- 1:nrow(DF)      #create an index number for each row
# 
# # For loop modified from
# # http://stackoverflow.com/questions/25797580/ggmap-mapdist-function-repeating-calculation-for-certain-o-d-pairs
# for (i in DF$row.number){
#   orig <- as.character(DF[i,c('from')]) # get origin from DF in the position line 'i', column 'from'
#   dest <- as.character(DF[i,c('to')])   # get origin from DF in the position line 'i', column 'to'
#   a <- mapdist(from = orig, to = dest, mode = "driving",output = "simple") # create temp. df 'a' with the output from mapdist
#   a$row.number <- i # include in temp. df 'a' the index number of the row from DF
#   DF$minutes[match(a$row.number, DF$row.number)] <- a$minutes # use the index number as a matching key to input/update the value of the variable 'minutes'
#   DF$seconds[match(a$row.number, DF$row.number)] <- a$seconds # ibdem DF$km[match(a$row.number, DF$row.number)] <- a$km #ibdem
#   DF$miles[match(a$row.number, DF$row.number)] <- a$miles # ibdem
# }
#   
# 
# write.csv(DF, "./data/From_515.csv")
# 
# 
# 
# 
# ## Lowell St Station
# # We will filter it down to E1 and E3 calls, which are mostly south east
# # We don't just do ones where they are first responder
# set.seed(124)
# 
# DF <- fd %>% 
#   filter(Unit == "E1" | Unit == "E3" &
#            is.na(X) == FALSE) %>% 
#   sample_n(1000) %>% 
#   mutate(to = paste(Y, X, sep = ", ")) %>% 
#   select(to, Full.Address) %>% 
#   group_by(Full.Address) %>% 
#   summarise(to = min(to), n = n()) %>% 
#   mutate(from = "651 Somerville Ave, Somerville, MA 02143",
#          minutes = "",
#          seconds = "",
#          miles = "")
# 
# DF$row.number <- 1:nrow(DF)      #create an index number for each row
# 
# # For loop modified from
# # http://stackoverflow.com/questions/25797580/ggmap-mapdist-function-repeating-calculation-for-certain-o-d-pairs
# for (i in DF$row.number){
#   orig <- as.character(DF[i,c('from')]) # get origin from DF in the position line 'i', column 'from'
#   dest <- as.character(DF[i,c('to')])   # get origin from DF in the position line 'i', column 'to'
#   a <- mapdist(from = orig, to = dest, mode = "driving",output = "simple") # create temp. df 'a' with the output from mapdist
#   a$row.number <- i # include in temp. df 'a' the index number of the row from DF
#   DF$minutes[match(a$row.number, DF$row.number)] <- a$minutes # use the index number as a matching key to input/update the value of the variable 'minutes'
#   DF$seconds[match(a$row.number, DF$row.number)] <- a$seconds # ibdem DF$km[match(a$row.number, DF$row.number)] <- a$km #ibdem
#   DF$miles[match(a$row.number, DF$row.number)] <- a$miles # ibdem
# }
# 
# 
# write.csv(DF, "./data/From_Lowell_Station.csv")
# 
# 
# 
# 
# 
# ## Joy and Washington
# # We will filter it down to E1 and E3 calls, which are mostly south east
# # We don't just do ones where they are first responder
# set.seed(124)
# 
# DF <- fd %>% 
#   filter(Unit == "E1" | Unit == "E3" &
#            is.na(X) == FALSE) %>% 
#   sample_n(1000) %>% 
#   mutate(to = paste(Y, X, sep = ", ")) %>% 
#   select(to, Full.Address) %>% 
#   group_by(Full.Address) %>% 
#   summarise(to = min(to), n = n()) %>% 
#   mutate(from = "146 Washington St, Somerville, MA 02143",
#          minutes = "",
#          seconds = "",
#          miles = "")
# 
# DF$row.number <- 1:nrow(DF)      #create an index number for each row
# 
# # For loop modified from
# # http://stackoverflow.com/questions/25797580/ggmap-mapdist-function-repeating-calculation-for-certain-o-d-pairs
# for (i in DF$row.number){
#   orig <- as.character(DF[i,c('from')]) # get origin from DF in the position line 'i', column 'from'
#   dest <- as.character(DF[i,c('to')])   # get origin from DF in the position line 'i', column 'to'
#   a <- mapdist(from = orig, to = dest, mode = "driving",output = "simple") # create temp. df 'a' with the output from mapdist
#   a$row.number <- i # include in temp. df 'a' the index number of the row from DF
#   DF$minutes[match(a$row.number, DF$row.number)] <- a$minutes # use the index number as a matching key to input/update the value of the variable 'minutes'
#   DF$seconds[match(a$row.number, DF$row.number)] <- a$seconds # ibdem DF$km[match(a$row.number, DF$row.number)] <- a$km #ibdem
#   DF$miles[match(a$row.number, DF$row.number)] <- a$miles # ibdem
# }
# 
# 
# write.csv(DF, "./data/From_Joy_and_Washington.csv")


#### Now Analyze ####
## TODO: re-elongate the data based on the n column, or do a weighted average
# (Although it's small enough that it will not likely make a difference)
FiveFifteen <- read.csv("./data/From_515.csv")
Lowell <- read.csv("./data/From_Lowell_Station.csv")

set.seed(124)
DF <- fd %>% 
  filter(Unit == "E1" | Unit == "E3" &
           is.na(X) == FALSE) %>% 
  sample_n(1000) %>% 
  select(Full.Address) %>% 
  group_by(Full.Address) %>% 
  summarise(n = n())

DF$from.FiveFifteen <- FiveFifteen$minutes
DF$from.Lowell <- Lowell$minutes

DF <- mutate(DF, Difference = from.FiveFifteen - from.Lowell)
summary(DF$Difference)
hist(DF$Difference)

### Summary: 515 would be better in these instances
# BUT !! I included E3's responses, so that was a given
# Perhaps I should rerun with just E1


# So let's try again with just E1
set.seed(124)
DF <- fd %>% 
  filter(Unit == "E1" | Unit == "E3" &
           is.na(X) == FALSE) %>% 
  sample_n(1000) %>% 
  select(Full.Address, Unit) %>% 
  group_by(Full.Address) %>% 
  summarise(n = n(), Unit = Unit[1])

DF$from.FiveFifteen <- FiveFifteen$minutes
DF$from.Lowell <- Lowell$minutes

DF <- DF %>% 
  mutate(Difference = from.FiveFifteen - from.Lowell) %>% 
  filter(Unit == "E1")
summary(DF$Difference)
hist(DF$Difference)

# Interesting: it's different, but remember these are cars that are going exactly the speed limit and stopping at all lights. the difference for firefighters is likely smaller!



# So let's try to get a sense of the scale by comparing to actual response times
set.seed(124)
DF <- fd %>% 
  filter(Unit == "E1" | Unit == "E3" &
           is.na(X) == FALSE) %>% 
  sample_n(1000) %>% 
  select(Full.Address, Unit, first.responder.response.time) %>% 
  group_by(Full.Address) %>% 
  summarise(n = n(), Unit = Unit[1],
            first.responder.response.time = mean(first.responder.response.time))

DF$from.FiveFifteen <- FiveFifteen$minutes
DF$from.Lowell <- Lowell$minutes

DF <- DF %>% 
  mutate(Difference = first.responder.response.time / from.Lowell) %>% 
  filter(Unit == "E1")
summary(DF$Difference)
hist(DF$Difference)

# so taking the median, it may be safe to assume that the difference between theoretical and actual = theoretical * .75, or so




#### Map the difference ####
FiveFifteen <- read.csv("./data/From_515.csv")
Lowell <- read.csv("./data/From_Lowell_Station.csv")


#All together
set.seed(124)
DF <- fd %>% 
  filter(Unit == "E1" | Unit == "E3" &
           is.na(X) == FALSE) %>% 
  sample_n(1000) %>% 
  select(Full.Address, X, Y) %>% 
  group_by(Full.Address) %>% 
  summarise(n = n(), Y = Y[1], X = X[1])

DF$from.FiveFifteen <- FiveFifteen$minutes
DF$from.Lowell <- Lowell$minutes

DF <- mutate(DF, Difference = from.FiveFifteen - from.Lowell)


map <- get_map(location = "Union Square, Somerville, MA", zoom=14, maptype="roadmap", color = "bw")
ggmap(map) + 
  geom_point(data = DF,
             aes(x = X, y = Y, colour = Difference, size = n)) +
  scale_colour_gradientn(name = "minutes", colours=(brewer.pal(9,"YlGnBu")), limits = c(-1.6, 0)) +
  scale_size(name = "calls", range=c(1,20)) +
  labs(fill="") + 
  theme_nothing(legend=TRUE) +
  ggtitle("Theoretical Difference Between 515 and Lowell")




#Just E1 calls
set.seed(124)
DF <- fd %>% 
  filter(Unit == "E1" | Unit == "E3" &
           is.na(X) == FALSE) %>% 
  sample_n(1000) %>% 
  select(Full.Address, X, Y, Unit) %>% 
  group_by(Full.Address) %>% 
  summarise(n = n(), Y = Y[1], X = X[1], Unit = Unit[1])

DF$from.FiveFifteen <- FiveFifteen$minutes
DF$from.Lowell <- Lowell$minutes

DF <- DF %>% 
  mutate(Difference = from.FiveFifteen - from.Lowell) %>% 
  filter(Unit == "E1" &
           Full.Address != "1 MUTUAL AID - CAMBRIDGE, Somerville, Massachusetts")


map <- get_map(location = "Union Square, Somerville, MA", zoom=14, maptype="roadmap", color = "bw")
ggmap(map) + 
  geom_point(data = DF,
             aes(x = X, y = Y, colour = Difference, size = 2)) +
  scale_colour_gradientn(name = "minutes", colours=(brewer.pal(9,"YlGnBu")), limits = c(-1.6, 0)) +
  scale_size(name = "calls", range=c(1,5)) +
  labs(fill="") + 
  theme_nothing(legend=TRUE) +
  ggtitle("Theoretical Difference Between 515 and Lowell")
