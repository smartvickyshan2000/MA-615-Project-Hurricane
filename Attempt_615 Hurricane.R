library(tidyverse)
library(maps)
library(sp)
library(sf)
library(leaflet)
library(htmlwidgets)
library(drat)
#addRepo("geanders")
#install.packages("hurricaneexposuredata")
#install.packages("hurricaneexposure")

library(hurricaneexposuredata)
library(hurricaneexposure)
data("hurr_tracks")
head(hurr_tracks)
#View(hurr_tracks)

######################### get the map of rainfall for Dolly-2008
Dolly_ht <- select(filter(as.data.frame(hurr_tracks), storm_id == "Dolly-2008"), -c(storm_id, usa_atcf_id, wind))
Dolly_rain <- select(filter(as.data.frame(rain), storm_id=="Dolly-2008"), -c(storm_id, usa_atcf_id))

# add up precipitation in each county
Dolly_rain_precip <- aggregate(precip~fips, Dolly_rain, sum)
Dolly_rain_precip$rainfall <- rep(NA, nrow(Dolly_rain_precip))

Dolly_rain_precip$`rainfall`[Dolly_rain_precip$precip>=0 & Dolly_rain_precip$precip<=25] <- 1
Dolly_rain_precip$`rainfall`[Dolly_rain_precip$precip> 25 & Dolly_rain_precip$precip<=50] <- 2
Dolly_rain_precip$`rainfall`[Dolly_rain_precip$precip> 50 & Dolly_rain_precip$precip<=75] <- 3
Dolly_rain_precip$`rainfall`[Dolly_rain_precip$precip> 75 & Dolly_rain_precip$precip<=100] <- 4
Dolly_rain_precip$`rainfall`[Dolly_rain_precip$precip> 100 & Dolly_rain_precip$precip<=125] <- 5
Dolly_rain_precip$`rainfall`[Dolly_rain_precip$precip> 125 & Dolly_rain_precip$precip<=150] <- 6
Dolly_rain_precip$`rainfall`[Dolly_rain_precip$precip> 150 & Dolly_rain_precip$precip<=175] <- 7
Dolly_rain_precip$`rainfall`[Dolly_rain_precip$precip> 175 & Dolly_rain_precip$precip<=200] <- 8
Dolly_rain_precip$`rainfall`[Dolly_rain_precip$precip>200] <- 9

Dolly_rain_precip$`rainfall` <- 
  factor(Dolly_rain_precip$`rainfall`, levels=c(1:9), 
         labels=c("[0,25]","(25,50]", "(50,75]", 
                  "(75,100]", "(100,125]","(125,150]",
                  "(150,175]","(175,200]","(200,207]"), ordered=TRUE)

# Turn fips into region(state) and subregion(county) columns
data(county.fips)
Dolly_rain_precip$fips <- as.double(Dolly_rain_precip$fips)
Dolly_rain_precip <- left_join(Dolly_rain_precip, county.fips, by="fips")
Dolly_rain_precip$region <- str_split(Dolly_rain_precip$polyname, ",", simplify=TRUE)[,1]
Dolly_rain_precip$subregion <- str_split(Dolly_rain_precip$polyname, ",", simplify=TRUE)[,2]
Dolly_rain_precip <- select(Dolly_rain_precip, -polyname)

# Import longitudes and latitudes data
Dolly_state <- map_data("state", region=unique(Dolly_rain_precip$region))
Dolly_county <- map_data("county", region=unique(Dolly_rain_precip$region))

Dolly_rain_precip <- left_join(Dolly_county, Dolly_rain_precip, by=c("region", "subregion"))

# Delete NA's
Dolly_rain_precip <- filter(Dolly_rain_precip, !is.na(`rainfall`))

# Dolly-2008 rainfall plot using ggplot2
ggplot() +
  geom_polygon(Dolly_rain_precip,mapping=aes(x=long, y=lat, group=group, fill=`rainfall`),color = "white") + 
  scale_fill_brewer(palette="Blues")+
  geom_path(data=Dolly_state, aes(x=long, y=lat, group=group), color="black")+
  geom_path(data=Dolly_ht, aes(x=longitude, y=latitude), color="red")+
  ggtitle("Dolly-2008")+
  theme(
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(hjust = 0.5,face = "bold")
  )

######################### get the map of wind speed for Dolly-2008
# use ggplot
Dolly_ht2 <- select(filter(as.data.frame(hurr_tracks), storm_id == "Dolly-2008"), -c(storm_id, usa_atcf_id))
Dolly_wind <- select(filter(as.data.frame(storm_winds), storm_id=="Dolly-2008"), -c(storm_id, usa_atcf_id, gust_dur, sust_dur))

# add up precipitation in each county
Dolly_wind_speed <- aggregate(vmax_gust ~ fips, Dolly_wind, sum)
Dolly_wind_speed$wind_speed <- rep(NA, nrow(Dolly_wind_speed))

Dolly_wind_speed$`wind_speed`[Dolly_wind_speed$vmax_gust>=0 & Dolly_wind_speed$vmax_gust<=15] <- 1
Dolly_wind_speed$`wind_speed`[Dolly_wind_speed$vmax_gust>15 & Dolly_wind_speed$vmax_gust<=20] <- 2
Dolly_wind_speed$`wind_speed`[Dolly_wind_speed$vmax_gust>20 & Dolly_wind_speed$vmax_gust<=25] <- 3
Dolly_wind_speed$`wind_speed`[Dolly_wind_speed$vmax_gust>25 & Dolly_wind_speed$vmax_gust<=30] <- 4
Dolly_wind_speed$`wind_speed`[Dolly_wind_speed$vmax_gust>30 & Dolly_wind_speed$vmax_gust<=35] <- 5
Dolly_wind_speed$`wind_speed`[Dolly_wind_speed$vmax_gust>35 & Dolly_wind_speed$vmax_gust<=40] <- 6
Dolly_wind_speed$`wind_speed`[Dolly_wind_speed$vmax_gust>40 & Dolly_wind_speed$vmax_gust<=45] <- 7
Dolly_wind_speed$`wind_speed`[Dolly_wind_speed$vmax_gust>45] <- 8

Dolly_wind_speed$`wind_speed` <- 
  factor(Dolly_wind_speed$`wind_speed`, levels=c(1:8), 
         labels=c("[0,15]","(15,20]", "(20,25]", 
                  "(25,30]", "(30,35]","(35,40]",
                  "(40,45]","(45,49.9]"), ordered=TRUE)

# Turn fips into region(state) and subregion(county) columns
data(county.fips)
Dolly_wind_speed$fips <- as.double(Dolly_wind_speed$fips)
Dolly_wind_speed <- left_join(Dolly_wind_speed, county.fips, by="fips")
Dolly_wind_speed$region <- str_split(Dolly_wind_speed$polyname, ",", simplify=TRUE)[,1]
Dolly_wind_speed$subregion <- str_split(Dolly_wind_speed$polyname, ",", simplify=TRUE)[,2]
Dolly_wind_speed <- select(Dolly_wind_speed, -polyname)

# Import longitudes and latitudes data
Dolly_state_wind <- map_data("state", region=unique(Dolly_wind_speed$region))
Dolly_county_wind <- map_data("county", region=unique(Dolly_wind_speed$region))

Dolly_wind_speed <- left_join(Dolly_county_wind, Dolly_wind_speed, by=c("region", "subregion"))

# Delete NA's
Dolly_wind_speed <- filter(Dolly_wind_speed, !is.na(`wind_speed`))

# Dolly-2008 wind_speed plot using ggplot2
ggplot() +
  geom_polygon(Dolly_wind_speed,mapping=aes(x=long, y=lat, group=group, fill=`wind_speed`),color = "white") + 
  scale_fill_brewer(palette="Reds")+
  geom_path(data=Dolly_state_wind, aes(x=long, y=lat, group=group), color="black")+
  geom_path(data=Dolly_ht2, aes(x=longitude, y=latitude), color="red")+
  ggtitle("Dolly-2008")+
  theme(
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(hjust = 0.5,face = "bold")
  )

######################### get the map of distance for Dolly-2008
# use ggplot
Dolly_distance <- select(filter(as.data.frame(closest_dist), storm_id=="Dolly-2008"), -c(storm_id, usa_atcf_id))
# add up distance in each county
Dolly_distance <- aggregate(storm_dist ~ fips, Dolly_distance, sum)
Dolly_distance$distance <- rep(NA, nrow(Dolly_distance))

Dolly_distance$`distance`[Dolly_distance$storm_dist>=0 & Dolly_distance$storm_dist<=500] <- 1
Dolly_distance$`distance`[Dolly_distance$storm_dist>500 & Dolly_distance$storm_dist<=1000] <- 2
Dolly_distance$`distance`[Dolly_distance$storm_dist>1000 & Dolly_distance$storm_dist<=1500] <- 3
Dolly_distance$`distance`[Dolly_distance$storm_dist>1500 & Dolly_distance$storm_dist<=2000] <- 4
Dolly_distance$`distance`[Dolly_distance$storm_dist>2000 & Dolly_distance$storm_dist<=2500] <- 5
Dolly_distance$`distance`[Dolly_distance$storm_dist>2500 & Dolly_distance$storm_dist<=3000] <- 6
Dolly_distance$`distance`[Dolly_distance$storm_dist>3000] <- 7

Dolly_distance$`distance` <- 
  factor(Dolly_distance$`distance`, levels=c(1:7), 
         labels=c("[0,500]","(500,1000]", "(1000,1500]", 
                  "(1500,2000]", "(2000,2500]","(2500,3000]",
                  "(3000,3350]"), ordered=TRUE)

# Turn fips into region(state) and subregion(county) columns
data(county.fips)
Dolly_distance$fips <- as.double(Dolly_distance$fips)
Dolly_distance <- left_join(Dolly_distance, county.fips, by="fips")
Dolly_distance$region <- str_split(Dolly_distance$polyname, ",", simplify=TRUE)[,1]
Dolly_distance$subregion <- str_split(Dolly_distance$polyname, ",", simplify=TRUE)[,2]
Dolly_distance <- select(Dolly_distance, -polyname)

# Import longitudes and latitudes data
Dolly_state_distance <- map_data("state", region=unique(Dolly_distance$region))
Dolly_county_distance <- map_data("county", region=unique(Dolly_distance$region))

Dolly_distance <- left_join(Dolly_county_distance, Dolly_distance, by=c("region", "subregion"))

# Delete NA's
Dolly_distance <- filter(Dolly_distance, !is.na(`distance`))

# Dolly-2008 distance plot using ggplot2
ggplot() +
  geom_polygon(Dolly_distance,mapping=aes(x=long, y=lat, group=group, fill=`distance`),color = "white") + 
  scale_fill_brewer(palette="Greens")+
  geom_path(data=Dolly_state_distance, aes(x=long, y=lat, group=group), color="black")+
  geom_path(data=Dolly_ht, aes(x=longitude, y=latitude), color="red")+
  ggtitle("Dolly-2008")+
  theme(
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(hjust = 0.5,face = "bold")
  )

