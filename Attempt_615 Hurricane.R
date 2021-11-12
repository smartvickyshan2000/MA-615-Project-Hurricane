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
                  "(150,175]","(175,200]","(200,222]"), ordered=TRUE)

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