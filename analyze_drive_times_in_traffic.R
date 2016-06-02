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




#### Geo Drive Time Analysis ####
fdg <- read.csv("./data/Fire_drive_times_w_traffic.csv") # fire data geo

# First make some variables to analyze the scale
# We will change these laters based on the scale
fdg <- fdg %>% 
  mutate(Union.and.Lowell = pmin(from.Union_in_traffic, from.Lowell_in_traffic, from.HQ_in_traffic, from.Highland_in_traffic, from.Teele_in_traffic)) %>%
  mutate(actual = first.responder.response.time + .001,
         predicted = Union.and.Lowell + .001) %>% # deal with 0 response times
  mutate(actual.v.predicted = actual - predicted,
         actual.v.predicted.per = actual / predicted) 
  


### SCALE ###
# This is a conservative estimate of how much faster firefighters arrive than they are predicted to

# First, here is the problem:
summary(fdg$actual.v.predicted)
# Actual takes about 15 seconds longer than predicted, on average
# This is also true if you take out the outliers:
avp_no_outliers <- fdg %>% 
  filter(actual.v.predicted > -3 & actual.v.predicted < 3.5) %>% 
  filter(actual.v.predicted.per > .5 & actual.v.predicted.per < 2)
summary(avp_no_outliers$actual.v.predicted)

# But that is not uniform
fit <- lm(actual.v.predicted.per ~ predicted, data=avp_no_outliers)
summary(fit) # show results
plot(avp_no_outliers$predicted, avp_no_outliers$actual.v.predicted.per)
# As the predicted time goes up, the amount you should scale it goes down
# So, for instance, you might double a short trip, but cut in 1/2 a predicted long trip
# Each additional minute that the trip is predicted to be, should lead to a 0.207 drop in the scaler, starting with 1 minute trip == 1.56 scaler
# But there is a flattening of the regression line on very long trips

# A function that uses the regression model above to scale down or up model response times based on google response times
scalerFunction <- function(x) {
  newdata <- data.frame(predicted=x)
  scale <- predict(fit, newdata, interval="predict")
  scale <- scale[1]
  scaled <- (x + .0001) * scale
  #large x the regression breakds down and the scaler becomes too small
  scaled.final <- ifelse(x > 4.5, x * .82, scaled)
  return(scaled.final)
}

# Now scale and add in other variables
for (i in 1:nrow(fdg)) {
fdg$from.HQ.scaled[i] <- scalerFunction(fdg$from.HQ_in_traffic[i])
fdg$from.Lowell.scaled[i] <- scalerFunction(fdg$from.Lowell_in_traffic[i])
fdg$from.FiveFifteen.scaled[i] <- scalerFunction(fdg$from.FiveFifteen_in_traffic[i])
fdg$from.Highland.scaled[i] <- scalerFunction(fdg$from.Highland_in_traffic[i])
fdg$from.Teele.scaled[i] <- scalerFunction(fdg$from.Teele_in_traffic[i])
fdg$from.JoyWashington.scaled[i] <- scalerFunction(fdg$from.JoyWashington_in_traffic[i])
fdg$from.Union.scaled[i] <- scalerFunction(fdg$from.Union_in_traffic[i])
}



# Now add in other variables
fdg <- fdg %>% 
  mutate(HQ.v.Lowell = from.HQ.scaled - from.Lowell.scaled,
         FiveFifteen.v.Lowell = from.FiveFifteen.scaled - from.Lowell.scaled,
         JW.v.FiveFifteen = from.JoyWashington.scaled - from.FiveFifteen.scaled,
         JW.v.Lowell = from.JoyWashington.scaled - from.Lowell.scaled,
         JW.and.Lowell = pmin(from.JoyWashington.scaled, from.Lowell.scaled, 
                              from.HQ.scaled, from.Highland.scaled, from.Teele.scaled),
         JW.and.FiveFifteen = pmin(from.JoyWashington.scaled, from.FiveFifteen.scaled, 
                                   from.HQ.scaled, from.Highland.scaled, from.Teele.scaled),
         Union.and.Lowell = pmin(from.Union.scaled, from.Lowell.scaled, from.HQ.scaled, 
                                 from.Highland.scaled, from.Teele.scaled),
         FiveFifteen.alone = pmin(from.FiveFifteen.scaled, from.HQ.scaled, 
                                  from.Highland.scaled, from.Teele.scaled)) %>%
  mutate(actual = first.responder.response.time + .001,
         predicted = Union.and.Lowell + .001) %>% # deal with 0 response times
  mutate(actual.v.predicted = actual - predicted,
         actual.v.predicted.per = actual / predicted)


### Get the fire station based on the minimum response time ###

# JW / 515
station_JW.and.FiveFifteen <- fdg %>% 
  select(from.JoyWashington_in_traffic, from.FiveFifteen_in_traffic, from.HQ_in_traffic, from.Highland_in_traffic, from.Teele_in_traffic)

# BEcause for some reason there is a max.col but not min.col
station_JW.and.FiveFifteen <- station_JW.and.FiveFifteen * -1

station_JW.and.FiveFifteen$station.JW.and.FiveFifteen <- colnames(station_JW.and.FiveFifteen)[max.col(station_JW.and.FiveFifteen,ties.method="random")]

# Now put it back in the base data
fdg$station.JW.and.FiveFifteen <- gsub("from.", "", station_JW.and.FiveFifteen$station.JW.and.FiveFifteen)


# JW / Lowell 
station_JW.and.Lowell <- fdg %>% 
  select(from.JoyWashington_in_traffic, from.Lowell_in_traffic, from.HQ_in_traffic, from.Highland_in_traffic, from.Teele_in_traffic)

# BEcause for some reason there is a max.col but not min.col
station_JW.and.Lowell <- station_JW.and.Lowell * -1

station_JW.and.Lowell$station.JW.and.Lowell <- colnames(station_JW.and.Lowell)[max.col(station_JW.and.Lowell,ties.method="random")]

# Now put it back in the base data
fdg$station.JW.and.Lowell <- gsub("from.", "", station_JW.and.Lowell$station.JW.and.Lowell)


# Current 
station_Union.and.Lowell <- fdg %>% 
  select(from.Union_in_traffic, from.Lowell_in_traffic, from.HQ_in_traffic, from.Highland_in_traffic, from.Teele_in_traffic)

# BEcause for some reason there is a max.col but not min.col
station_Union.and.Lowell <- station_Union.and.Lowell * -1

station_Union.and.Lowell$station.Union.and.Lowell <- colnames(station_Union.and.Lowell)[max.col(station_Union.and.Lowell,ties.method="random")]

# Now put it back in the base data
fdg$station.Union.and.Lowell <- gsub("from.", "", station_Union.and.Lowell$station.Union.and.Lowell)


rm(station_Union.and.Lowell, station_JW.and.Lowell, station_JW.and.FiveFifteen)



## SCALE
# This is what I used to get the scale of X above
# If you want to see how it works first comment out the code where the scale is applied to the data:
# this line: fdg[cols] <- fdg[cols] * scale
# First let's get a sense of the scale: how quickly fire actually arrived vs. the model
scale <- fdg %>% 
  select(Union.and.Lowell, first.responder.response.time) %>% 
  mutate(diff = Union.and.Lowell / first.responder.response.time) %>% 
  filter(diff < 3)  #take out outliers based on a histogram

summary(scale$diff)
rm(scale)
## / SCALE




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



# Remove stuff to stay tidy
rm(fit, i, avp_no_outliers, scalerFunction)




#### Maps of Drive Time ####

### Model of response times ###

# Model Response of Lowell and Joy/Wash
fdg_map <- fdg %>% 
  group_by(Full.Address) %>% 
  summarise(n=n(), JW.and.Lowell = mean(JW.and.Lowell), 
            X = X[1], Y = Y[1]) %>%
  mutate(under.five = ifelse(JW.and.Lowell > 5, "No", "Yes"),
         minutes = JW.and.Lowell) %>% 
  mutate(Predicted = ifelse(minutes > 0 & minutes <= 2, "0-2",
                            ifelse(minutes > 2 & minutes <= 4, "2-4",
                                   ifelse(minutes > 4 & minutes <= 5, "4-5",
                                          "5 +"))))

map <- get_map(location = "Somerville, MA", zoom=14, maptype="roadmap", color = "bw")
ggmap(map) + 
  geom_point(data = fdg_map,
             aes(x = X, y = Y, colour = JW.and.Lowell, size = n), alpha = .7) +
  scale_colour_gradientn(name = "minutes", colours=(brewer.pal(9,"YlGnBu")), limits = c(0,5)) +
  scale_size(name = "calls", range=c(3,15)) +
  labs(fill="") + 
  theme_nothing(legend=TRUE) +
  ggtitle("Predicted Time from East of Union and Lowell")

ggsave("./plots/drive_time_in_traffic/all_from_Lowell_and_JW_1.png", dpi=250, width=6, height=5)


map <- get_map(location = "Somerville, MA", zoom=13, maptype="roadmap", color = "bw")
ggmap(map) + 
  geom_point(data = fdg_map,
             aes(x = X, y = Y, colour = JW.and.Lowell, size = n), alpha = .7) +
  scale_colour_gradientn(name = "minutes", colours=(brewer.pal(9,"YlGnBu")), limits = c(0,5)) +
  scale_size(name = "calls", range=c(3,15)) +
  labs(fill="") + 
  theme_nothing(legend=TRUE) +
  ggtitle("Predicted Time from East of Union and Lowell")

ggsave("./plots/drive_time_in_traffic/all_from_Lowell_and_JW_2.png", dpi=250, width=6, height=5)


map <- get_map(location = "Somerville, MA", zoom=14, maptype="roadmap", color = "bw")
ggmap(map) + 
  geom_point(data = fdg_map,
             aes(x = X, y = Y, colour = Predicted, size = n), alpha = .6) +
  # scale_colour_gradientn(name = "minutes", colours=(brewer.pal(9,"YlGnBu")), limits = c(0,5)) +
  scale_colour_manual(values=c("green", "#56B4E9", "#000080","red")) +
  scale_size(name = "calls", range=c(3,15)) +
  labs(fill="") + 
  theme_nothing(legend=TRUE) +
  # facet_wrap(~ Predicted) +
  ggtitle("Predicted Time from East of Union and Lowell")

ggsave("./plots/drive_time_in_traffic/all_from_Lowell_and_JW_3.png", dpi=250, width=6, height=5)


map <- get_map(location = "Somerville, MA", zoom=13, maptype="roadmap", color = "bw")
ggmap(map) + 
  geom_point(data = fdg_map,
             aes(x = X, y = Y, colour = Predicted, size = n), alpha = .6) +
  # scale_colour_gradientn(name = "minutes", colours=(brewer.pal(9,"YlGnBu")), limits = c(0,5)) +
  scale_colour_manual(values=c("green", "#56B4E9", "#000080","red")) +
  scale_size(name = "calls", range=c(3,15)) +
  labs(fill="") + 
  theme_nothing(legend=TRUE) +
  # facet_wrap(~ Predicted) +
  ggtitle("Predicted Time from East of Union and Lowell")

ggsave("./plots/drive_time_in_traffic/all_from_Lowell_and_JW_4.png", dpi=250, width=6, height=5)




# Model Response of 515 and Joy/Wash
fdg_map <- fdg %>% 
  group_by(Full.Address) %>% 
  summarise(n=n(), JW.and.FiveFifteen = mean(JW.and.FiveFifteen), 
            X = X[1], Y = Y[1]) %>%
  mutate(under.five = ifelse(JW.and.FiveFifteen > 5, "No", "Yes"),
         minutes = JW.and.FiveFifteen) %>% 
  mutate(Predicted = ifelse(minutes > 0 & minutes <= 2, "0-2",
                            ifelse(minutes > 2 & minutes <= 4, "2-4",
                                   ifelse(minutes > 4 & minutes <= 5, "4-5",
                                          "5 +"))))

map <- get_map(location = "Somerville, MA", zoom=14, maptype="roadmap", color = "bw")
ggmap(map) + 
  geom_point(data = fdg_map,
             aes(x = X, y = Y, colour = JW.and.FiveFifteen, size = n), alpha = .7) +
  scale_colour_gradientn(name = "minutes", colours=(brewer.pal(9,"YlGnBu")), limits = c(0,5)) +
  scale_size(name = "calls", range=c(3,15)) +
  labs(fill="") + 
  theme_nothing(legend=TRUE) +
  ggtitle("Predicted Time from East of Union and 515")

ggsave("./plots/drive_time_in_traffic/all_from_515_and_JW_1.png", dpi=250, width=6, height=5)


map <- get_map(location = "Somerville, MA", zoom=13, maptype="roadmap", color = "bw")
ggmap(map) + 
  geom_point(data = fdg_map,
             aes(x = X, y = Y, colour = JW.and.FiveFifteen, size = n), alpha = .7) +
  scale_colour_gradientn(name = "minutes", colours=(brewer.pal(9,"YlGnBu")), limits = c(0,5)) +
  scale_size(name = "calls", range=c(3,15)) +
  labs(fill="") + 
  theme_nothing(legend=TRUE) +
  ggtitle("Predicted Time from East of Union and 515")

ggsave("./plots/drive_time_in_traffic/all_from_515_and_JW_2.png", dpi=250, width=6, height=5)


map <- get_map(location = "Somerville, MA", zoom=14, maptype="roadmap", color = "bw")
ggmap(map) + 
  geom_point(data = fdg_map,
             aes(x = X, y = Y, colour = Predicted, size = n), alpha = .6) +
  # scale_colour_gradientn(name = "minutes", colours=(brewer.pal(9,"YlGnBu")), limits = c(0,5)) +
  scale_colour_manual(values=c("green", "#56B4E9", "#000080","red")) +
  scale_size(name = "calls", range=c(3,15)) +
  labs(fill="") + 
  theme_nothing(legend=TRUE) +
  # facet_wrap(~ Predicted) +
  ggtitle("Predicted Time from East of Union and 515")

ggsave("./plots/drive_time_in_traffic/all_from_515_and_JW_3.png", dpi=250, width=6, height=5)


map <- get_map(location = "Somerville, MA", zoom=13, maptype="roadmap", color = "bw")
ggmap(map) + 
  geom_point(data = fdg_map,
             aes(x = X, y = Y, colour = Predicted, size = n), alpha = .6) +
  # scale_colour_gradientn(name = "minutes", colours=(brewer.pal(9,"YlGnBu")), limits = c(0,5)) +
  scale_colour_manual(values=c("green", "#56B4E9", "#000080","red")) +
  scale_size(name = "calls", range=c(3,15)) +
  labs(fill="") + 
  theme_nothing(legend=TRUE) +
  # facet_wrap(~ Predicted) +
  ggtitle("Predicted Time from East of Union and 515")

ggsave("./plots/drive_time_in_traffic/all_from_515_and_JW_4.png", dpi=250, width=6, height=5)




# Model Response of 515 alone
fdg_map <- fdg %>% 
  group_by(Full.Address) %>% 
  summarise(n=n(), FiveFifteen.alone = mean(FiveFifteen.alone), 
            X = X[1], Y = Y[1]) %>%
  mutate(under.five = ifelse(FiveFifteen.alone > 5, "No", "Yes"),
         minutes = FiveFifteen.alone) %>% 
  mutate(Predicted = ifelse(minutes > 0 & minutes <= 2, "0-2",
                            ifelse(minutes > 2 & minutes <= 4, "2-4",
                                   ifelse(minutes > 4 & minutes <= 5, "4-5",
                                          "5 +"))))

map <- get_map(location = "Somerville, MA", zoom=14, maptype="roadmap", color = "bw")
ggmap(map) + 
  geom_point(data = fdg_map,
             aes(x = X, y = Y, colour = FiveFifteen.alone, size = n), alpha = .7) +
  scale_colour_gradientn(name = "minutes", colours=(brewer.pal(9,"YlGnBu")), limits = c(0,5)) +
  scale_size(name = "calls", range=c(3,15)) +
  labs(fill="") + 
  theme_nothing(legend=TRUE) +
  ggtitle("Predicted Time from 515")

ggsave("./plots/drive_time_in_traffic/all_from_515_alone.png", dpi=250, width=6, height=5)


map <- get_map(location = "Somerville, MA", zoom=13, maptype="roadmap", color = "bw")
ggmap(map) + 
  geom_point(data = fdg_map,
             aes(x = X, y = Y, colour = FiveFifteen.alone, size = n), alpha = .7) +
  scale_colour_gradientn(name = "minutes", colours=(brewer.pal(9,"YlGnBu")), limits = c(0,5)) +
  scale_size(name = "calls", range=c(3,15)) +
  labs(fill="") + 
  theme_nothing(legend=TRUE) +
  ggtitle("Predicted Time from 515")

ggsave("./plots/drive_time_in_traffic/all_from_515_alone_2.png", dpi=250, width=6, height=5)


map <- get_map(location = "Somerville, MA", zoom=14, maptype="roadmap", color = "bw")
ggmap(map) + 
  geom_point(data = fdg_map,
             aes(x = X, y = Y, colour = Predicted, size = n), alpha = .6) +
  # scale_colour_gradientn(name = "minutes", colours=(brewer.pal(9,"YlGnBu")), limits = c(0,5)) +
  scale_colour_manual(values=c("green", "#56B4E9", "#000080","red")) +
  scale_size(name = "calls", range=c(3,15)) +
  labs(fill="") + 
  theme_nothing(legend=TRUE) +
  # facet_wrap(~ Predicted) +
  ggtitle("Predicted Time from 515")

ggsave("./plots/drive_time_in_traffic/all_from_515_alone_3.png", dpi=250, width=6, height=5)


map <- get_map(location = "Somerville, MA", zoom=13, maptype="roadmap", color = "bw")
ggmap(map) + 
  geom_point(data = fdg_map,
             aes(x = X, y = Y, colour = Predicted, size = n), alpha = .6) +
  # scale_colour_gradientn(name = "minutes", colours=(brewer.pal(9,"YlGnBu")), limits = c(0,5)) +
  scale_colour_manual(values=c("green", "#56B4E9", "#000080","red")) +
  scale_size(name = "calls", range=c(3,15)) +
  labs(fill="") + 
  theme_nothing(legend=TRUE) +
  # facet_wrap(~ Predicted) +
  ggtitle("Predicted Time from 515")

ggsave("./plots/drive_time_in_traffic/all_from_515_alone_4.png", dpi=250, width=6, height=5)





# Actual response of Union and Lowell
fdg_map <- fdg %>% 
  group_by(Full.Address) %>% 
  summarise(n=n(), actual = mean(first.responder.response.time), 
            X = X[1], Y = Y[1]) %>%
  mutate(under.five = ifelse(actual > 5, "No", "Yes"))

map <- get_map(location = "Somerville, MA", zoom=14, maptype="roadmap", color = "bw")
ggmap(map) + 
  geom_point(data = fdg_map,
             aes(x = X, y = Y, colour = actual, size = n), alpha = .7) +
  scale_colour_gradientn(name = "minutes", colours=(brewer.pal(9,"YlGnBu")), limits = c(0,5)) +
  scale_size(name = "calls", range=c(3,15)) +
  labs(fill="") + 
  theme_nothing(legend=TRUE) +
  ggtitle("Actual Time from Union and Lowell")

ggsave("./plots/drive_time_in_traffic/all_current_1.png", dpi=250, width=6, height=5)


map <- get_map(location = "Somerville, MA", zoom=13, maptype="roadmap", color = "bw")
ggmap(map) + 
  geom_point(data = fdg_map,
             aes(x = X, y = Y, colour = actual, size = n), alpha = .7) +
  scale_colour_gradientn(name = "minutes", colours=(brewer.pal(9,"YlGnBu")), limits = c(0,5)) +
  scale_size(name = "calls", range=c(3,15)) +
  labs(fill="") + 
  theme_nothing(legend=TRUE) +
  ggtitle("Actual Time from Union and Lowell")

ggsave("./plots/drive_time_in_traffic/all_current_2.png", dpi=250, width=6, height=5)




# Predicted response of Union and Lowell
fdg_map <- fdg %>% 
  group_by(Full.Address) %>% 
  summarise(n=n(), Union.and.Lowell = mean(Union.and.Lowell), 
            X = X[1], Y = Y[1]) %>%
  mutate(under.five = ifelse(Union.and.Lowell > 5, "No", "Yes"),
         minutes = Union.and.Lowell) %>% 
  mutate(Predicted = ifelse(minutes > 0 & minutes <= 2, "0-2",
                            ifelse(minutes > 2 & minutes <= 4, "2-4",
                                   ifelse(minutes > 4 & minutes <= 5, "4-5",
                                          "5 +"))))

map <- get_map(location = "Somerville, MA", zoom=14, maptype="roadmap", color = "bw")
ggmap(map) + 
  geom_point(data = fdg_map,
             aes(x = X, y = Y, colour = Union.and.Lowell, size = n), alpha = .7) +
  scale_colour_gradientn(name = "minutes", colours=(brewer.pal(9,"YlGnBu")), limits = c(0,5)) +
  scale_size(name = "calls", range=c(3,15)) +
  labs(fill="") + 
  theme_nothing(legend=TRUE) +
  ggtitle("Predicted Time from Union and Lowell")

ggsave("./plots/drive_time_in_traffic/all_current_predicted_1.png", dpi=250, width=6, height=5)


map <- get_map(location = "Somerville, MA", zoom=13, maptype="roadmap", color = "bw")
ggmap(map) + 
  geom_point(data = fdg_map,
             aes(x = X, y = Y, colour = Union.and.Lowell, size = n), alpha = .7) +
  scale_colour_gradientn(name = "minutes", colours=(brewer.pal(9,"YlGnBu")), limits = c(0,5)) +
  scale_size(name = "calls", range=c(3,15)) +
  labs(fill="") + 
  theme_nothing(legend=TRUE) +
  ggtitle("Predicted Time from Union and Lowell")

ggsave("./plots/drive_time_in_traffic/all_current_predicted_2.png", dpi=250, width=6, height=5)


map <- get_map(location = "Somerville, MA", zoom=14, maptype="roadmap", color = "bw")
ggmap(map) + 
  geom_point(data = fdg_map,
             aes(x = X, y = Y, colour = Predicted, size = n), alpha = .6) +
  # scale_colour_gradientn(name = "minutes", colours=(brewer.pal(9,"YlGnBu")), limits = c(0,5)) +
  scale_colour_manual(values=c("green", "#56B4E9", "#000080","red")) +
  scale_size(name = "calls", range=c(3,15)) +
  labs(fill="") + 
  theme_nothing(legend=TRUE) +
  # facet_wrap(~ Predicted) +
  ggtitle("Predicted Time from Union and Lowell")

ggsave("./plots/drive_time_in_traffic/all_current_predicted_3.png", dpi=250, width=6, height=5)


map <- get_map(location = "Somerville, MA", zoom=13, maptype="roadmap", color = "bw")
ggmap(map) + 
  geom_point(data = fdg_map,
             aes(x = X, y = Y, colour = Predicted, size = n), alpha = .6) +
  # scale_colour_gradientn(name = "minutes", colours=(brewer.pal(9,"YlGnBu")), limits = c(0,5)) +
  scale_colour_manual(values=c("green", "#56B4E9", "#000080","red")) +
  scale_size(name = "calls", range=c(3,15)) +
  labs(fill="") + 
  theme_nothing(legend=TRUE) +
  # facet_wrap(~ Predicted) +
  ggtitle("Predicted Time from Union and Lowell")

ggsave("./plots/drive_time_in_traffic/all_current_predicted_4.png", dpi=250, width=6, height=5)



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

ggsave("./plots/drive_time_in_traffic/closest_station_current.png", dpi=250, width=6, height=5)



# Optimal boundaries Joy/Wash & 515
fdg_map <- fdg 

map <- get_map(location = "Somerville, MA", zoom=14, maptype="roadmap", color = "bw")
ggmap(map) + 
  geom_point(data = fdg_map,
             aes(x = X, y = Y, bins = 26, colour = fdg_map$station.JW.and.FiveFifteen), 
             size = 4, alpha = .3) +
  labs(fill="", colour = "Closest") + 
  theme_nothing(legend=TRUE) +
  ggtitle("Predicted Closest Station: East of Union & 515")

ggsave("./plots/drive_time_in_traffic/closest_JW_FiveFifteen.png", dpi=250, width=6, height=5)



# Optimal boundaries East of Union & Lowell
fdg_map <- fdg 

map <- get_map(location = "Somerville, MA", zoom=14, maptype="roadmap", color = "bw")
ggmap(map) + 
  geom_point(data = fdg_map,
             aes(x = X, y = Y, bins = 26, colour = fdg_map$station.JW.and.Lowell), 
             size = 4, alpha = .3) +
  labs(fill="", colour = "Closest") + 
  theme_nothing(legend=TRUE) +
  ggtitle("Predicted Closest Station: East of Union & Lowell")

ggsave("./plots/drive_time_in_traffic/closest_JW_Lowell.png", dpi=250, width=6, height=5)



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

ggsave("./plots/drive_time_in_traffic/Difference_Joy_Wash_and_515.png", dpi=250, width=6, height=5)



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

ggsave("./plots/drive_time_in_traffic/Difference_Joy_Wash_and_Lowell.png", dpi=250, width=6, height=5)



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

ggsave("./plots/drive_time_in_traffic/Difference_515_and_Lowell.png", dpi=250, width=6, height=5)



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

ggsave("./plots/drive_time_in_traffic/Difference_515_and_Lowell2.png", dpi=250, width=6, height=5)



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

ggsave("./plots/drive_time_in_traffic/Difference_HQ_and_Lowell.png", dpi=250, width=6, height=5)



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

ggsave("./plots/drive_time_in_traffic/from_515_to_E3.png", dpi=250, width=6, height=5)



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

ggsave("./plots/drive_time_in_traffic/from_HQ_to_Lowell.png", dpi=250, width=6, height=5)



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

ggsave("./plots/drive_time_in_traffic/from_JW_to_E3.png", dpi=250, width=6, height=5)



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
  ggtitle("Predicted Time from East of Union to E3 Area")

ggsave("./plots/drive_time_in_traffic/from_JW_to_E3_binary.png", dpi=250, width=6, height=5)



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
  ggtitle("Predicted Time from East of Union to E3 Area")

ggsave("./plots/drive_time_in_traffic/from_Lowell_to_E3_binary.png", dpi=250, width=6, height=5)




# Actual vs Predicted
# Darker is where it took them longer than expected
fdg_map <- fdg %>% 
  group_by(Full.Address) %>% 
  summarise(n=n(), actual.v.predicted = mean(actual.v.predicted), 
            X = X[1], Y = Y[1])

map <- get_map(location = "Somerville, MA", zoom=14, maptype="roadmap", color = "bw")
ggmap(map) + 
  geom_point(data = fdg_map,
             aes(x = X, y = Y, colour = actual.v.predicted, size = n)) +
  scale_colour_gradientn(name = "minutes", colours=(brewer.pal(9,"YlGnBu")), limits = c(-3,3)) +
  scale_size(name = "calls", range=c(3,15)) +
  labs(fill="") + 
  theme_nothing(legend=TRUE) +
  ggtitle("Actual vs Predicted Arrival Times")

ggsave("./plots/drive_time_in_traffic/actual_v_predicted.png", dpi=250, width=6, height=5)


