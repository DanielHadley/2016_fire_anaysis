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




#### Geo Drive Time Analysis ####
fdg <- read.csv("./data/Fire_drive_times.csv") # fire data geo


# First let's get a sense of the scale: how quickly fire actually arrived vs. the model
scale <- fdg %>% 
  filter(Unit == "E1") %>% 
  select(from.Lowell, first.responder.response.time) %>% 
  mutate(diff = from.Lowell / first.responder.response.time) %>% 
  filter(diff < 3)  #take out outliers based on a histogram

summary(scale$diff)

# it's similar to when I did it before
# I'll just take something between the mean and median: .75
scale = .75

# Now scale them
cols <- c("from.HQ", "from.Lowell", "from.JoyWashington", "from.FiveFifteen")
fdg[cols] <- fdg[cols] * scale


fdg <- fdg %>% 
  mutate(HQ.v.Lowell = from.HQ - from.Lowell,
         FiveFifteen.v.Lowell = from.FiveFifteen - from.Lowell,
         JW.v.FiveFifteen = from.JoyWashington - from.FiveFifteen,
         JW.v.Lowell = from.JoyWashington - from.Lowell,
         JW.and.Lowell = pmin(from.JoyWashington, from.Lowell, from.HQ, from.Highland, from.Teele),
         JW.and.FiveFifteen = pmin(from.JoyWashington, from.FiveFifteen, from.HQ, from.Highland, from.Teele),
         Union.and.Lowell = pmin(from.Union, from.Lowell, from.HQ, from.Highland, from.Teele))



### Get the fire station based on the minimum response time ###

# JW / 515
station_JW.and.FiveFifteen <- fdg %>% 
  select(from.JoyWashington, from.FiveFifteen, from.HQ, from.Highland, from.Teele)

# BEcause for some reason there is a max.col but not min.col
station_JW.and.FiveFifteen <- station_JW.and.FiveFifteen * -1

station_JW.and.FiveFifteen$station.JW.and.FiveFifteen <- colnames(station_JW.and.FiveFifteen)[max.col(station_JW.and.FiveFifteen,ties.method="random")]

# Now put it back in the base data
fdg$station.JW.and.FiveFifteen <- gsub("from.", "", station_JW.and.FiveFifteen$station.JW.and.FiveFifteen)


# JW / Lowell 
station_JW.and.Lowell <- fdg %>% 
  select(from.JoyWashington, from.Lowell, from.HQ, from.Highland, from.Teele)

# BEcause for some reason there is a max.col but not min.col
station_JW.and.Lowell <- station_JW.and.Lowell * -1

station_JW.and.Lowell$station.JW.and.Lowell <- colnames(station_JW.and.Lowell)[max.col(station_JW.and.Lowell,ties.method="random")]

# Now put it back in the base data
fdg$station.JW.and.Lowell <- gsub("from.", "", station_JW.and.Lowell$station.JW.and.Lowell)


# Current 
station_Union.and.Lowell <- fdg %>% 
  select(from.Union, from.Lowell, from.HQ, from.Highland, from.Teele)

# BEcause for some reason there is a max.col but not min.col
station_Union.and.Lowell <- station_Union.and.Lowell * -1

station_Union.and.Lowell$station.Union.and.Lowell <- colnames(station_Union.and.Lowell)[max.col(station_Union.and.Lowell,ties.method="random")]

# Now put it back in the base data
fdg$station.Union.and.Lowell <- gsub("from.", "", station_Union.and.Lowell$station.Union.and.Lowell)


rm(station_Union.and.Lowell, station_JW.and.Lowell, station_JW.and.FiveFifteen)





### Some summary stats ###

# 515 would help with Union Square calls
summary(fdg$JW.v.FiveFifteen[fdg$Unit == "E3"])
hist(fdg$JW.v.FiveFifteen[fdg$Unit == "E3"])

# Lowell would not help with Union Square calls as much
summary(fdg$JW.v.Lowell[fdg$Unit == "E3"])
hist(fdg$JW.v.Lowell[fdg$Unit == "E3"])

# 515 would be better than Lowell for E1 calls
# But only slightly
summary(fdg$FiveFifteen.v.Lowell[fdg$Unit == "E1"])
hist(fdg$FiveFifteen.v.Lowell[fdg$Unit == "E1"])


# Overall 515 is sightly better
summary(fdg$JW.and.Lowell)
hist(fdg$JW.and.Lowell)

summary(fdg$JW.and.FiveFifteen)
hist(fdg$JW.and.FiveFifteen)

# But either scenario is better than current status:
summary(fdg$Union.and.Lowell)


## Actual v predicted
fdg <- fdg %>% mutate(actual.v.predicted = first.responder.response.time - Union.and.Lowell)
summary(fdg$actual.v.predicted)
hist(fdg$actual.v.predicted)



#### Maps of Drive Time ####

### Model of response times ###

# Model Response of Lowell and Joy/Wash
fdg_map <- fdg %>% 
  group_by(Full.Address) %>% 
  summarise(n=n(), JW.and.Lowell = mean(JW.and.Lowell), 
            X = X[1], Y = Y[1]) %>%
  mutate(under.five = ifelse(JW.and.Lowell > 5, "No", "Yes"))

map <- get_map(location = "Somerville, MA", zoom=14, maptype="roadmap", color = "bw")
ggmap(map) + 
  geom_point(data = fdg_map,
             aes(x = X, y = Y, colour = JW.and.Lowell, size = n)) +
  scale_colour_gradientn(name = "minutes", colours=(brewer.pal(9,"YlGnBu")), limits = c(0,5)) +
  scale_size(name = "calls", range=c(3,15)) +
  labs(fill="") + 
  theme_nothing(legend=TRUE) +
  ggtitle("Predicted Time from Joy/Wash and Lowell")

ggsave("./plots/all_from_Lowell_and_JW_1.png", dpi=250, width=6, height=5)

map <- get_map(location = "Somerville, MA", zoom=13, maptype="roadmap", color = "bw")
ggmap(map) + 
  geom_point(data = fdg_map,
             aes(x = X, y = Y, colour = JW.and.Lowell, size = n)) +
  scale_colour_gradientn(name = "minutes", colours=(brewer.pal(9,"YlGnBu")), limits = c(0,5)) +
  scale_size(name = "calls", range=c(3,15)) +
  labs(fill="") + 
  theme_nothing(legend=TRUE) +
  ggtitle("Predicted Time from Joy/Wash and Lowell")

ggsave("./plots/all_from_Lowell_and_JW_2.png", dpi=250, width=6, height=5)



# Model Response of 515 and Joy/Wash
fdg_map <- fdg %>% 
  group_by(Full.Address) %>% 
  summarise(n=n(), JW.and.FiveFifteen = mean(JW.and.FiveFifteen), 
            X = X[1], Y = Y[1]) %>%
  mutate(under.five = ifelse(JW.and.FiveFifteen > 5, "No", "Yes"))

map <- get_map(location = "Somerville, MA", zoom=14, maptype="roadmap", color = "bw")
ggmap(map) + 
  geom_point(data = fdg_map,
             aes(x = X, y = Y, colour = JW.and.FiveFifteen, size = n)) +
  scale_colour_gradientn(name = "minutes", colours=(brewer.pal(9,"YlGnBu")), limits = c(0,5)) +
  scale_size(name = "calls", range=c(3,15)) +
  labs(fill="") + 
  theme_nothing(legend=TRUE) +
  ggtitle("Predicted Time from Joy/Wash and 515")

ggsave("./plots/all_from_515_and_JW_1.png", dpi=250, width=6, height=5)

map <- get_map(location = "Somerville, MA", zoom=13, maptype="roadmap", color = "bw")
ggmap(map) + 
  geom_point(data = fdg_map,
             aes(x = X, y = Y, colour = JW.and.FiveFifteen, size = n)) +
  scale_colour_gradientn(name = "minutes", colours=(brewer.pal(9,"YlGnBu")), limits = c(0,5)) +
  scale_size(name = "calls", range=c(3,15)) +
  labs(fill="") + 
  theme_nothing(legend=TRUE) +
  ggtitle("Predicted Time from Joy/Wash and 515")

ggsave("./plots/all_from_515_and_JW_2.png", dpi=250, width=6, height=5)



# Actual response of Union and Lowell
fdg_map <- fdg %>% 
  group_by(Full.Address) %>% 
  summarise(n=n(), actual = mean(first.responder.response.time), 
            X = X[1], Y = Y[1]) %>%
  mutate(under.five = ifelse(actual > 5, "No", "Yes"))

map <- get_map(location = "Somerville, MA", zoom=14, maptype="roadmap", color = "bw")
ggmap(map) + 
  geom_point(data = fdg_map,
             aes(x = X, y = Y, colour = actual, size = n)) +
  scale_colour_gradientn(name = "minutes", colours=(brewer.pal(9,"YlGnBu")), limits = c(0,5)) +
  scale_size(name = "calls", range=c(3,15)) +
  labs(fill="") + 
  theme_nothing(legend=TRUE) +
  ggtitle("Actual Time from Union and Lowell")

ggsave("./plots/all_current_1.png", dpi=250, width=6, height=5)

map <- get_map(location = "Somerville, MA", zoom=13, maptype="roadmap", color = "bw")
ggmap(map) + 
  geom_point(data = fdg_map,
             aes(x = X, y = Y, colour = actual, size = n)) +
  scale_colour_gradientn(name = "minutes", colours=(brewer.pal(9,"YlGnBu")), limits = c(0,5)) +
  scale_size(name = "calls", range=c(3,15)) +
  labs(fill="") + 
  theme_nothing(legend=TRUE) +
  ggtitle("Actual Time from Union and Lowell")

ggsave("./plots/all_current_2.png", dpi=250, width=6, height=5)



# Predicted response of Union and Lowell
fdg_map <- fdg %>% 
  group_by(Full.Address) %>% 
  summarise(n=n(), Union.and.Lowell = mean(Union.and.Lowell), 
            X = X[1], Y = Y[1]) %>%
  mutate(under.five = ifelse(Union.and.Lowell > 5, "No", "Yes"))

map <- get_map(location = "Somerville, MA", zoom=14, maptype="roadmap", color = "bw")
ggmap(map) + 
  geom_point(data = fdg_map,
             aes(x = X, y = Y, colour = Union.and.Lowell, size = n)) +
  scale_colour_gradientn(name = "minutes", colours=(brewer.pal(9,"YlGnBu")), limits = c(0,5)) +
  scale_size(name = "calls", range=c(3,15)) +
  labs(fill="") + 
  theme_nothing(legend=TRUE) +
  ggtitle("Predicted Time from Union and Lowell")

ggsave("./plots/all_current_predicted_1.png", dpi=250, width=6, height=5)

map <- get_map(location = "Somerville, MA", zoom=13, maptype="roadmap", color = "bw")
ggmap(map) + 
  geom_point(data = fdg_map,
             aes(x = X, y = Y, colour = Union.and.Lowell, size = n)) +
  scale_colour_gradientn(name = "minutes", colours=(brewer.pal(9,"YlGnBu")), limits = c(0,5)) +
  scale_size(name = "calls", range=c(3,15)) +
  labs(fill="") + 
  theme_nothing(legend=TRUE) +
  ggtitle("Predicted Time from Union and Lowell")

ggsave("./plots/all_current_predicted_2.png", dpi=250, width=6, height=5)




### Optimal Boundaries maps ###

# Optimal boundaries current
fdg_map <- fdg 

map <- get_map(location = "Somerville, MA", zoom=14, maptype="roadmap", color = "bw")
ggmap(map) + 
  geom_point(data = fdg_map,
             aes(x = X, y = Y, bins = 26, colour = fdg_map$station.Union.and.Lowell), 
             size = 4, alpha = .3) +
  labs(fill="", colour = "Closest") + 
  theme_nothing(legend=TRUE) +
  ggtitle("Predicted Closest Station: Current")

ggsave("./plots/closest_station_current.png", dpi=250, width=6, height=5)



# Optimal boundaries Joy/Wash & 515
fdg_map <- fdg 

map <- get_map(location = "Somerville, MA", zoom=14, maptype="roadmap", color = "bw")
ggmap(map) + 
  geom_point(data = fdg_map,
             aes(x = X, y = Y, bins = 26, colour = fdg_map$station.JW.and.FiveFifteen), 
             size = 4, alpha = .3) +
  labs(fill="", colour = "Closest") + 
  theme_nothing(legend=TRUE) +
  ggtitle("Predicted Closest Station: Joy/Wash & 515")

ggsave("./plots/closest_JW_FiveFifteen.png", dpi=250, width=6, height=5)



# Optimal boundaries Joy/Wash & Lowell
fdg_map <- fdg 

map <- get_map(location = "Somerville, MA", zoom=14, maptype="roadmap", color = "bw")
ggmap(map) + 
  geom_point(data = fdg_map,
             aes(x = X, y = Y, bins = 26, colour = fdg_map$station.JW.and.Lowell), 
             size = 4, alpha = .3) +
  labs(fill="", colour = "Closest") + 
  theme_nothing(legend=TRUE) +
  ggtitle("Predicted Closest Station: Joy/Wash & Lowell")

ggsave("./plots/closest_JW_Lowell.png", dpi=250, width=6, height=5)



### Difference Between maps ###

#Theoretical Difference Between 515 and Joy/Washington
fdg_map <- fdg %>% 
  filter(Unit == "E3") %>% 
  group_by(Full.Address) %>% 
  summarise(n=n(), JW.v.FiveFifteen = mean(JW.v.FiveFifteen), X = X[1], Y = Y[1])

map <- get_map(location = "Union Square, Somerville, MA", zoom=15, maptype="roadmap", color = "bw")
ggmap(map) + 
  geom_point(data = fdg_map,
             aes(x = X, y = Y, colour = fdg_map$JW.v.FiveFifteen, size = n)) +
  scale_colour_gradientn(name = "minutes", colours=(brewer.pal(9,"YlGnBu"))) +
  scale_size(name = "calls", range=c(3,15)) +
  labs(fill="") + 
  theme_nothing(legend=TRUE) +
  ggtitle("Predicted Difference Between Joy/Washington and 515")

ggsave("./plots/Difference_Joy_Wash_and_515.png", dpi=250, width=6, height=5)



#Theoretical Difference Between Lowell and Joy/Washington
fdg_map <- fdg %>% 
  filter(Unit == "E3") %>% 
  group_by(Full.Address) %>% 
  summarise(n=n(), JW.v.Lowell = mean(JW.v.Lowell), X = X[1], Y = Y[1])

map <- get_map(location = "Union Square, Somerville, MA", zoom=15, maptype="roadmap", color = "bw")
ggmap(map) + 
  geom_point(data = fdg_map,
             aes(x = X, y = Y, colour = fdg_map$JW.v.Lowell, size = n)) +
  scale_colour_gradientn(name = "minutes", colours=(brewer.pal(9,"YlGnBu"))) +
  scale_size(name = "calls", range=c(3,15)) +
  labs(fill="") + 
  theme_nothing(legend=TRUE) +
  ggtitle("Predicted Difference Between Joy/Washington and Lowell")

ggsave("./plots/Difference_Joy_Wash_and_Lowell.png", dpi=250, width=6, height=5)



#Theoretical Difference Between Lowell and 515
fdg_map <- fdg %>% 
  # filter(Unit == "E1") %>% 
  group_by(Full.Address) %>% 
  summarise(n=n(), FiveFifteen.v.Lowell = mean(FiveFifteen.v.Lowell), X = X[1], Y = Y[1])

map <- get_map(location = "651 Somerville Ave, Somerville, MA", zoom=15, maptype="roadmap", color = "bw")
ggmap(map) + 
  geom_point(data = fdg_map,
             aes(x = X, y = Y, colour = FiveFifteen.v.Lowell, size = n)) +
  scale_colour_gradientn(name = "minutes", colours=(brewer.pal(9,"YlGnBu"))) +
  scale_size(name = "calls", range=c(3,15)) +
  labs(fill="") + 
  theme_nothing(legend=TRUE) +
  ggtitle("Predicted Difference Between 515 and Lowell")

ggsave("./plots/Difference_515_and_Lowell.png", dpi=250, width=6, height=5)



#Theoretical Difference Between Lowell and 515
fdg_map <- fdg %>% 
  filter(Unit == "E1") %>% 
  group_by(Full.Address) %>% 
  summarise(n=n(), FiveFifteen.v.Lowell = mean(FiveFifteen.v.Lowell), X = X[1], Y = Y[1]) %>% 
  mutate(is.515.Faster = ifelse(FiveFifteen.v.Lowell > 0, "No", "Yes"))

map <- get_map(location = "651 Somerville Ave, Somerville, MA", zoom=15, maptype="roadmap", color = "bw")
ggmap(map) + 
  geom_point(data = fdg_map,
             aes(x = X, y = Y, colour = is.515.Faster, size = n)) +
  scale_size(name = "calls", range=c(3,15)) +
  labs(fill="") + 
  theme_nothing(legend=TRUE) +
  ggtitle("Predicted Difference Between 515 and Lowell")

ggsave("./plots/Difference_515_and_Lowell2.png", dpi=250, width=6, height=5)



#Theoretical Difference Between HQ and Lowell
fdg_map <- fdg %>% 
  filter(Unit == "E1") %>% 
  group_by(Full.Address) %>% 
  summarise(n=n(), HQ.v.Lowell = mean(HQ.v.Lowell), X = X[1], Y = Y[1])

map <- get_map(location = "651 Somerville Ave, Somerville, MA", zoom=15, maptype="roadmap", color = "bw")
ggmap(map) + 
  geom_point(data = fdg_map,
             aes(x = X, y = Y, colour = HQ.v.Lowell, size = n)) +
  scale_colour_gradientn(name = "minutes", colours=(brewer.pal(9,"YlGnBu"))) +
  scale_size(name = "calls", range=c(3,15)) +
  labs(fill="") + 
  theme_nothing(legend=TRUE) +
  ggtitle("Predicted Difference Between HQ and Lowell")

ggsave("./plots/Difference_HQ_and_Lowell.png", dpi=250, width=6, height=5)



### From to maps ###

#From 515 to E3
fdg_map <- fdg %>% 
  filter(Unit == "E3") %>% 
  group_by(Full.Address) %>% 
  summarise(n=n(), from.Lowell = mean(from.Lowell), X = X[1], Y = Y[1])

map <- get_map(location = "Union Square, Somerville, MA", zoom=15, maptype="roadmap", color = "bw")
ggmap(map) + 
  geom_point(data = fdg_map,
             aes(x = X, y = Y, colour = from.Lowell, size = n)) +
  scale_colour_gradientn(name = "minutes", colours=(brewer.pal(9,"YlGnBu"))) +
  scale_size(name = "calls", range=c(3,15)) +
  labs(fill="") + 
  theme_nothing(legend=TRUE) +
  ggtitle("Predicted Time from 515 to E3 Area")

ggsave("./plots/from_515_to_E3.png", dpi=250, width=6, height=5)



# from HQ to E1
fdg_map <- fdg %>% 
  filter(Unit == "E1") %>% 
  group_by(Full.Address) %>% 
  summarise(n=n(), from.HQ = mean(from.HQ), X = X[1], Y = Y[1])

map <- get_map(location = "651 Somerville Ave, Somerville, MA", zoom=15, maptype="roadmap", color = "bw")
ggmap(map) + 
  geom_point(data = fdg_map,
             aes(x = X, y = Y, colour = from.HQ, size = n)) +
  scale_colour_gradientn(name = "minutes", colours=(brewer.pal(9,"YlGnBu"))) +
  scale_size(name = "calls", range=c(3,15)) +
  labs(fill="") + 
  theme_nothing(legend=TRUE) +
  ggtitle("Predicted Time from HQ to E1 Area")

ggsave("./plots/from_HQ_to_Lowell.png", dpi=250, width=6, height=5)



# from JW to E3
fdg_map <- fdg %>% 
  filter(Unit == "E3") %>% 
  group_by(Full.Address) %>% 
  summarise(n=n(), from.JoyWashington = mean(from.JoyWashington), X = X[1], Y = Y[1])

map <- get_map(location = "Union Square, Somerville, MA", zoom=15, maptype="roadmap", color = "bw")
ggmap(map) + 
  geom_point(data = fdg_map,
             aes(x = X, y = Y, colour = from.JoyWashington, size = n)) +
  scale_colour_gradientn(name = "minutes", colours=(brewer.pal(9,"YlGnBu"))) +
  scale_size(name = "calls", range=c(3,15)) +
  labs(fill="") + 
  theme_nothing(legend=TRUE) +
  ggtitle("Predicted Time from Joy/Wash to E3 Area")

ggsave("./plots/from_JW_to_E3.png", dpi=250, width=6, height=5)



# from JW to E3
fdg_map <- fdg %>% 
  filter(Unit == "E3") %>% 
  group_by(Full.Address) %>% 
  summarise(n=n(), from.JoyWashington = mean(from.JoyWashington), X = X[1], Y = Y[1]) %>% 
  mutate(under.five = ifelse(from.JoyWashington > 5, "No", "Yes"))

map <- get_map(location = "Union Square, Somerville, MA", zoom=15, maptype="roadmap", color = "bw")
ggmap(map) + 
  geom_point(data = fdg_map,
             aes(x = X, y = Y, colour = under.five, size = n)) +
  scale_size(name = "calls", range=c(3,15)) +
  labs(fill="") + 
  theme_nothing(legend=TRUE) +
  ggtitle("Predicted Time from Joy/Wash to E3 Area")

ggsave("./plots/from_JW_to_E3_binary.png", dpi=250, width=6, height=5)



# from Lowell to E3
fdg_map <- fdg %>% 
  filter(Unit == "E3") %>% 
  group_by(Full.Address) %>% 
  summarise(n=n(), from.Lowell = mean(from.Lowell), X = X[1], Y = Y[1]) %>% 
  mutate(under.five = ifelse(from.Lowell > 5, "No", "Yes"))

map <- get_map(location = "Union Square, Somerville, MA", zoom=15, maptype="roadmap", color = "bw")
ggmap(map) + 
  geom_point(data = fdg_map,
             aes(x = X, y = Y, colour = under.five, size = n)) +
  scale_size(name = "calls", range=c(3,15)) +
  labs(fill="") + 
  theme_nothing(legend=TRUE) +
  ggtitle("Predicted Time from Joy/Wash to E3 Area")

ggsave("./plots/from_Lowell_to_E3_binary.png", dpi=250, width=6, height=5)



#Theoretical Response of Lowell and Joy/Wash
fdg_map <- fdg %>% 
  group_by(Full.Address) %>% 
  summarise(n=n(), from.Lowell = mean(from.Lowell), 
            from.JoyWashington = mean(from.JoyWashington), 
            X = X[1], Y = Y[1]) %>%
  mutate(Quickest = pmin(from.JoyWashington, from.Lowell)) %>% 
  mutate(under.five = ifelse(Quickest > 5, "No", "Yes"))

map <- get_map(location = "Union Square, Somerville, MA", zoom=14, maptype="roadmap", color = "bw")
ggmap(map) + 
  geom_point(data = fdg_map,
             aes(x = X, y = Y, colour = under.five, size = n)) +
  scale_size(name = "calls", range=c(3,15)) +
  labs(fill="") + 
  theme_nothing(legend=TRUE) +
  ggtitle("Predicted Time from Joy/Wash and Lowell")

ggsave("./plots/from_Lowell_and_JW_binary.png", dpi=250, width=6, height=5)

ggmap(map) + 
  geom_point(data = fdg_map,
             aes(x = X, y = Y, colour = Quickest, size = n)) +
  scale_colour_gradientn(name = "minutes", colours=(brewer.pal(9,"YlGnBu")), limits = c(0,5)) +
  scale_size(name = "calls", range=c(3,15)) +
  labs(fill="") + 
  theme_nothing(legend=TRUE) +
  ggtitle("Predicted Time from Joy/Wash and Lowell")

ggsave("./plots/from_Lowell_and_JW.png", dpi=250, width=6, height=5)




#Theoretical Response of 515 and Joy/Wash
fdg_map <- fdg %>% 
  group_by(Full.Address) %>% 
  summarise(n=n(), from.FiveFifteen = mean(from.FiveFifteen), 
            from.JoyWashington = mean(from.JoyWashington), 
            X = X[1], Y = Y[1]) %>%
  mutate(Quickest = pmin(from.JoyWashington, from.FiveFifteen)) %>% 
  mutate(under.five = ifelse(Quickest > 5, "No", "Yes"))

map <- get_map(location = "Union Square, Somerville, MA", zoom=14, maptype="roadmap", color = "bw")
ggmap(map) + 
  geom_point(data = fdg_map,
             aes(x = X, y = Y, colour = under.five, size = n)) +
  scale_size(name = "calls", range=c(3,15)) +
  labs(fill="") + 
  theme_nothing(legend=TRUE) +
  ggtitle("Predicted Time from Joy/Wash and 515")

ggsave("./plots/from_FiveFifteen_and_JW_binary.png", dpi=250, width=6, height=5)

ggmap(map) + 
  geom_point(data = fdg_map,
             aes(x = X, y = Y, colour = Quickest, size = n)) +
  scale_colour_gradientn(name = "minutes", colours=(brewer.pal(9,"YlGnBu")), limits = c(0,5)) +
  scale_size(name = "calls", range=c(3,15)) +
  labs(fill="") + 
  theme_nothing(legend=TRUE) +
  ggtitle("Predicted Time from Joy/Wash and 515")

ggsave("./plots/from_FiveFifteen_and_JW.png", dpi=250, width=6, height=5)