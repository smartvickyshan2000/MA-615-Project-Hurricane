library(tidyverse)
library(maps)
library(sp)
library(sf)
library(leaflet)
library(htmlwidgets)
library(drat)
library(gstat)
#addRepo("geanders")
#install.packages("hurricaneexposuredata")
#install.packages("hurricaneexposure")

library(hurricaneexposuredata)
library(hurricaneexposure)
data("hurr_tracks")
head(hurr_tracks)

Dolly_v <- filter(as.data.frame(hurr_tracks), storm_id == "Dolly-2008")

coordinates(Dolly_v) <- ~ latitude + longitude

vc <- variogram(object=wind~1, Dolly_v, cloud=T)

plot(vc)

vm <- variogram(object=wind~1, Dolly_v, cutoff=10, width=0.3, map=T)

plot(vm)


#this is all of our point pairs

v <- variogram(object=wind~1, Dolly_v)
emp <- fit.variogram(v, model=vgm(psill=800, nugget=10, range=9, model="Gau"))  

plot(v, emp, main="Semivariogram of Wind Speed", pch=20)

View(BUOYS_6HOUR_SNAPSHOT)
BUOYS_6HOUR_SNAPSHOT$lat<-as.numeric(BUOYS_6HOUR_SNAPSHOT$lat)
BUOYS_6HOUR_SNAPSHOT$long<-as.numeric(BUOYS_6HOUR_SNAPSHOT$long)
coordinates(BUOYS_6HOUR_SNAPSHOT) <-~lat+long  
v2<-variogram(object=WSPD_adjusted~1, data=BUOYS_6HOUR_SNAPSHOT)

str(BUOYS_6HOUR_SNAPSHOT$lat)