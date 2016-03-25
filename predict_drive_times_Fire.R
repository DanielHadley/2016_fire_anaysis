# Created By Daniel Hadley Fri Mar 25 10:48:57 EDT 2016 #
# Take geocoded data and make predictions for drive times from all possible fire stations
# This will allow us to model different station locations

setwd("/Users/DHadley/Github/2016_fire_anaysis/")
setwd("/Users/dhadley/Documents/GitHub/2016_fire_anaysis/")

# library(devtools)
# install_github("DanielHadley/ggmapAPIkey")

library(dplyr)
library(lubridate)
library(tidyr)
library(broom) # augments d with model variables
library(ggplot2)
library(ggmap)
library(RColorBrewer)




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




#### Data prep ####
# This should hopefully get us a simple model of resonse times from various locations

# We filter the dates because E1 moved to Lowell
# And then things got weird in 2015

# THis will be the base that we add data columns to
set.seed(124)
base_DF <- fd %>% 
  filter(Nature.of.Call %in% response_time_incidents) %>% 
  filter(bad.geocode == 0 &
           is.first.responder == 1) %>% 
  filter(Year %in% c(2012, 2013, 2014)) %>% 
  sample_n(3000)


# Lowell Station
DF <- base_DF %>% 
  mutate(to = paste(Y, X, sep = ", ")) %>% 
  select(to, Full.Address) %>% 
  group_by(Full.Address) %>% 
  summarise(to = min(to), n = n()) %>% 
  mutate(from = "651 Somerville Ave, Somerville, MA",
         minutes = "",
         seconds = "",
         miles = "")

DF$row.number <- 1:nrow(DF)      #create an index number for each row

# For loop modified from
# http://stackoverflow.com/questions/25797580/ggmap-mapdist-function-repeating-calculation-for-certain-o-d-pairs
for (i in DF$row.number){
  orig <- as.character(DF[i,c('from')]) # get origin from DF in the position line 'i', column 'from'
  dest <- as.character(DF[i,c('to')])   # get origin from DF in the position line 'i', column 'to'
  a <- mapdist(from = orig, to = dest, mode = "driving",output = "simple") # create temp. df 'a' with the output from mapdist
  a$row.number <- i # include in temp. df 'a' the index number of the row from DF
  DF$minutes[match(a$row.number, DF$row.number)] <- a$minutes # use the index number as a matching key to input/update the value of the variable 'minutes'
  DF$seconds[match(a$row.number, DF$row.number)] <- a$seconds # ibdem DF$km[match(a$row.number, DF$row.number)] <- a$km #ibdem
  DF$miles[match(a$row.number, DF$row.number)] <- a$miles # ibdem
}
  
# Now merge it back
toCombine <- DF %>% 
  mutate(from.Lowell = minutes) %>% 
  select(Full.Address, from.Lowell)

base_DF_Final <- merge(base_DF, toCombine)

write.csv(base_DF_Final, "./data/Fire_drive_times.csv")


# HQ
DF <- base_DF %>% 
  mutate(to = paste(Y, X, sep = ", ")) %>% 
  select(to, Full.Address) %>% 
  group_by(Full.Address) %>% 
  summarise(to = min(to), n = n()) %>% 
  mutate(from = "266 Broadway, Somerville, MA",
         minutes = "",
         seconds = "",
         miles = "")

DF$row.number <- 1:nrow(DF)      #create an index number for each row

# For loop modified from
# http://stackoverflow.com/questions/25797580/ggmap-mapdist-function-repeating-calculation-for-certain-o-d-pairs
for (i in DF$row.number){
  orig <- as.character(DF[i,c('from')]) # get origin from DF in the position line 'i', column 'from'
  dest <- as.character(DF[i,c('to')])   # get origin from DF in the position line 'i', column 'to'
  a <- mapdist(from = orig, to = dest, mode = "driving",output = "simple") # create temp. df 'a' with the output from mapdist
  a$row.number <- i # include in temp. df 'a' the index number of the row from DF
  DF$minutes[match(a$row.number, DF$row.number)] <- a$minutes # use the index number as a matching key to input/update the value of the variable 'minutes'
  DF$seconds[match(a$row.number, DF$row.number)] <- a$seconds # ibdem DF$km[match(a$row.number, DF$row.number)] <- a$km #ibdem
  DF$miles[match(a$row.number, DF$row.number)] <- a$miles # ibdem
}

# Now merge it back
toCombine <- DF %>% 
  mutate(from.HQ = minutes) %>% 
  select(Full.Address, from.HQ)

base_DF_Final <- merge(base_DF_Final, toCombine)


# 515
DF <- base_DF %>% 
  mutate(to = paste(Y, X, sep = ", ")) %>% 
  select(to, Full.Address) %>% 
  group_by(Full.Address) %>% 
  summarise(to = min(to), n = n()) %>% 
  mutate(from = "515 Somerville Ave, Somerville, MA",
         minutes = "",
         seconds = "",
         miles = "")

DF$row.number <- 1:nrow(DF)      #create an index number for each row

# For loop modified from
# http://stackoverflow.com/questions/25797580/ggmap-mapdist-function-repeating-calculation-for-certain-o-d-pairs
for (i in DF$row.number){
  orig <- as.character(DF[i,c('from')]) # get origin from DF in the position line 'i', column 'from'
  dest <- as.character(DF[i,c('to')])   # get origin from DF in the position line 'i', column 'to'
  a <- mapdist(from = orig, to = dest, mode = "driving",output = "simple") # create temp. df 'a' with the output from mapdist
  a$row.number <- i # include in temp. df 'a' the index number of the row from DF
  DF$minutes[match(a$row.number, DF$row.number)] <- a$minutes # use the index number as a matching key to input/update the value of the variable 'minutes'
  DF$seconds[match(a$row.number, DF$row.number)] <- a$seconds # ibdem DF$km[match(a$row.number, DF$row.number)] <- a$km #ibdem
  DF$miles[match(a$row.number, DF$row.number)] <- a$miles # ibdem
}

# Now merge it back
toCombine <- DF %>% 
  mutate(from.FiveFifteen = minutes) %>% 
  select(Full.Address, from.FiveFifteen)

base_DF_Final <- merge(base_DF_Final, toCombine)


# Joy and Washington
DF <- base_DF %>% 
  mutate(to = paste(Y, X, sep = ", ")) %>% 
  select(to, Full.Address) %>% 
  group_by(Full.Address) %>% 
  summarise(to = min(to), n = n()) %>% 
  mutate(from = "146 Washington St, Somerville, MA",
         minutes = "",
         seconds = "",
         miles = "")

DF$row.number <- 1:nrow(DF)      #create an index number for each row

# For loop modified from
# http://stackoverflow.com/questions/25797580/ggmap-mapdist-function-repeating-calculation-for-certain-o-d-pairs
for (i in DF$row.number){
  orig <- as.character(DF[i,c('from')]) # get origin from DF in the position line 'i', column 'from'
  dest <- as.character(DF[i,c('to')])   # get origin from DF in the position line 'i', column 'to'
  a <- mapdist(from = orig, to = dest, mode = "driving",output = "simple") # create temp. df 'a' with the output from mapdist
  a$row.number <- i # include in temp. df 'a' the index number of the row from DF
  DF$minutes[match(a$row.number, DF$row.number)] <- a$minutes # use the index number as a matching key to input/update the value of the variable 'minutes'
  DF$seconds[match(a$row.number, DF$row.number)] <- a$seconds # ibdem DF$km[match(a$row.number, DF$row.number)] <- a$km #ibdem
  DF$miles[match(a$row.number, DF$row.number)] <- a$miles # ibdem
}

# Now merge it back
toCombine <- DF %>% 
  mutate(from.JoyWashington = minutes) %>% 
  select(Full.Address, from.JoyWashington)

base_DF_Final <- merge(base_DF_Final, toCombine)

write.csv(base_DF_Final, "./data/Fire_drive_times.csv")


# Highland
DF <- base_DF %>% 
  mutate(to = paste(Y, X, sep = ", ")) %>% 
  select(to, Full.Address) %>% 
  group_by(Full.Address) %>% 
  summarise(to = min(to), n = n()) %>% 
  mutate(from = "265 Highland Ave, Somerville, MA",
         minutes = "",
         seconds = "",
         miles = "")

DF$row.number <- 1:nrow(DF)      #create an index number for each row

# For loop modified from
# http://stackoverflow.com/questions/25797580/ggmap-mapdist-function-repeating-calculation-for-certain-o-d-pairs
for (i in DF$row.number){
  orig <- as.character(DF[i,c('from')]) # get origin from DF in the position line 'i', column 'from'
  dest <- as.character(DF[i,c('to')])   # get origin from DF in the position line 'i', column 'to'
  a <- mapdist(from = orig, to = dest, mode = "driving",output = "simple") # create temp. df 'a' with the output from mapdist
  a$row.number <- i # include in temp. df 'a' the index number of the row from DF
  DF$minutes[match(a$row.number, DF$row.number)] <- a$minutes # use the index number as a matching key to input/update the value of the variable 'minutes'
  DF$seconds[match(a$row.number, DF$row.number)] <- a$seconds # ibdem DF$km[match(a$row.number, DF$row.number)] <- a$km #ibdem
  DF$miles[match(a$row.number, DF$row.number)] <- a$miles # ibdem
}

# Now merge it back
toCombine <- DF %>% 
  mutate(from.Highland = minutes) %>% 
  select(Full.Address, from.Highland)

base_DF_Final <- merge(base_DF_Final, toCombine)

write.csv(base_DF_Final, "./data/Fire_drive_times.csv")


# Teele
DF <- base_DF %>% 
  mutate(to = paste(Y, X, sep = ", ")) %>% 
  select(to, Full.Address) %>% 
  group_by(Full.Address) %>% 
  summarise(to = min(to), n = n()) %>% 
  mutate(from = "238 Holland St Somerville, MA",
         minutes = "",
         seconds = "",
         miles = "")

DF$row.number <- 1:nrow(DF)      #create an index number for each row

# For loop modified from
# http://stackoverflow.com/questions/25797580/ggmap-mapdist-function-repeating-calculation-for-certain-o-d-pairs
for (i in DF$row.number){
  orig <- as.character(DF[i,c('from')]) # get origin from DF in the position line 'i', column 'from'
  dest <- as.character(DF[i,c('to')])   # get origin from DF in the position line 'i', column 'to'
  a <- mapdist(from = orig, to = dest, mode = "driving",output = "simple") # create temp. df 'a' with the output from mapdist
  a$row.number <- i # include in temp. df 'a' the index number of the row from DF
  DF$minutes[match(a$row.number, DF$row.number)] <- a$minutes # use the index number as a matching key to input/update the value of the variable 'minutes'
  DF$seconds[match(a$row.number, DF$row.number)] <- a$seconds # ibdem DF$km[match(a$row.number, DF$row.number)] <- a$km #ibdem
  DF$miles[match(a$row.number, DF$row.number)] <- a$miles # ibdem
}

# Now merge it back
toCombine <- DF %>% 
  mutate(from.Teele = minutes) %>% 
  select(Full.Address, from.Teele)

base_DF_Final <- merge(base_DF_Final, toCombine)

write.csv(base_DF_Final, "./data/Fire_drive_times.csv")


# Union
DF <- base_DF %>% 
  mutate(to = paste(Y, X, sep = ", ")) %>% 
  select(to, Full.Address) %>% 
  group_by(Full.Address) %>% 
  summarise(to = min(to), n = n()) %>% 
  mutate(from = "243 Somerville Ave, Somerville, MA",
         minutes = "",
         seconds = "",
         miles = "")

DF$row.number <- 1:nrow(DF)      #create an index number for each row

# For loop modified from
# http://stackoverflow.com/questions/25797580/ggmap-mapdist-function-repeating-calculation-for-certain-o-d-pairs
for (i in DF$row.number){
  orig <- as.character(DF[i,c('from')]) # get origin from DF in the position line 'i', column 'from'
  dest <- as.character(DF[i,c('to')])   # get origin from DF in the position line 'i', column 'to'
  a <- mapdist(from = orig, to = dest, mode = "driving",output = "simple") # create temp. df 'a' with the output from mapdist
  a$row.number <- i # include in temp. df 'a' the index number of the row from DF
  DF$minutes[match(a$row.number, DF$row.number)] <- a$minutes # use the index number as a matching key to input/update the value of the variable 'minutes'
  DF$seconds[match(a$row.number, DF$row.number)] <- a$seconds # ibdem DF$km[match(a$row.number, DF$row.number)] <- a$km #ibdem
  DF$miles[match(a$row.number, DF$row.number)] <- a$miles # ibdem
}

# Now merge it back
toCombine <- DF %>% 
  mutate(from.Union = minutes) %>% 
  select(Full.Address, from.Union)

base_DF_Final <- merge(base_DF_Final, toCombine)

write.csv(base_DF_Final, "./data/Fire_drive_times.csv")