library(tidyverse)
library(maps)
library(sp)
library(sf)
library(drat)
library(gstat)
library(hurricaneexposuredata)
library(hurricaneexposure)
data("hurr_tracks")
source("01_ImportBuoy.R")

Dolly_v <- filter(as.data.frame(hurr_tracks), storm_id == "Dolly-2008")

coordinates(Dolly_v) <- ~ latitude + longitude

#bubble(Dolly_v,zcol='wind',fill=TRUE,do.sqrt=FALSE,maxsize=3)

v <- variogram(object=wind~1, Dolly_v)
emp <- fit.variogram(v, model=vgm(psill=800, nugget=10, range=9, model="Gau"))  
BUOYS_6HOUR_SNAPSHOT <- BUOYS_6HOUR_SNAPSHOT %>% mutate(long = as.numeric(long), lat = as.numeric(lat)) %>% mutate(long2 = -1*long)

coordinates(BUOYS_6HOUR_SNAPSHOT) <-~lat+long  

v2<-variogram(object=WSPD_adjusted~1, data=BUOYS_6HOUR_SNAPSHOT)

emp2<-fit.variogram(v, model=vgm(psill=80, nugget=55, range=2.0, model="Gau")) 

plot(v, emp, main="Semivariogram of 1 min Max Wind Speed (Hurr_Tracks Data)", pch=20,col="red")

#This is a semivariogram that shows that generally as distance between points in the Hurr tracks dataset increases, the difference in their maximum sustained wind speed also increases, indicating a possible spatial relationship. Extrapolating from this plot further is difficult because the data violates the stationarity assumption required for kriging, which states that different regions contained in the data cannot have their outcome values changing at different rates than others, and a hurricane that moves around a region means that change is occurring at different rates depending on where it is at a given moment.

plot(v2, emp2, main="Semivariogram of Average Wind Speed (Buoy Data)", pch=20,col="blue")

#This semivariogram does not have a traditional pattern like in the first example, the observed nugget value is high, indicating that there is a lot of noise in the data, and we can not say if there is a possible spatial relationship between buoy distance and average weed speed. 


coordinates(Dolly_rain_precip) <- ~lat + long
#bubble(Dolly_rain_precip,zcol='precip',fill=TRUE,do.sqrt=FALSE,maxsize=3)

v_rain <- variogram(object=precip~1, Dolly_rain_precip)
emp_rain <- fit.variogram(v_rain, model=vgm(psill=1200, nugget=170, range=14, model="Gau"))  
 
plot(v_rain, emp_rain, main="Semivariogram of daily rainfall given 5 days before and 3 days after ", pch=20,col="red")

#This is a semivariogram that shows that generally as distance between points in the rain dataset increases, the difference in their maximum sustained rainfall also increases, indicating a possible spatial relationship. This is no related rainfall information in Buoys information. 