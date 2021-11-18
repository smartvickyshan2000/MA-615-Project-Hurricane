#Buoy Maps
source("Attempt_615 Hurricane.R", echo = FALSE)
source(file = "01_ImportBuoy.R", echo = FALSE)

#Map of wind speed with Buoys plotted on them.


BUOYS_6HOUR_SNAPSHOT <- BUOYS_6HOUR_SNAPSHOT %>% mutate(long = as.numeric(long), lat = as.numeric(lat)) %>% mutate(long2 = -1*long)


ggplot() +
  geom_polygon(Dolly_wind_speed,mapping=aes(x=long, y=lat, group=group, fill=`wind_speed`),color = "white") + 
  scale_fill_brewer(palette="Reds")+
  geom_path(data=Dolly_state_wind, aes(x=long, y=lat, group=group), color="black")+
  geom_path(data=Dolly_ht2, aes(x=longitude, y=latitude), color="red")+
  geom_point(data = BUOYS_6HOUR_SNAPSHOT, aes(x = long2,y = lat), color = "blue", shape = 10, size = 2)+
  ggtitle("Dolly-2008")+
  theme(
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(hjust = 0.5,face = "bold")
  )



#1

Dolly_Buoy <- inner_join(Dolly_ht2, BUOYS_AVERAGE, by =  "date")
Dolly_Buoy %<>% mutate(upper = WSPD_MEAN+WSPD_SD, lower = WSPD_MEAN-WSPD_SD)

ggplot(data = Dolly_Buoy) + geom_point(mapping=aes(x=date, y = wind), color = "red", shape= 10) +
  geom_point(mapping= aes(date, y = WSPD_MEAN), color = "blue", shape= 10) +
  geom_point(mapping=aes(date, y = upper), color = "blue", shape =95, size = 3) +
  geom_point(mapping=aes(date, y = lower), color = "blue", shape =95, size = 3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ylab("Wind Speed") + xlab("Date")  + 
  labs(title = "Comparing Wind Speeds From Buoys and Hurr Tracks", subtitle= "Red = Hurr Tracks, Blue = Average across buoys")


#4
Dolly_Buoy2 <- inner_join(Dolly_ht2, BUOYS_6HOUR_SNAPSHOT, by =  "date")
Dolly_Buoy2$BUOY_ID_F <- ifelse(Dolly_Buoy2$BUOY_ID == "rsjt2",2,
                                ifelse(Dolly_Buoy2$BUOY_ID == "babt2",3,
                                       ifelse(Dolly_Buoy2$BUOY_ID == "mqtt2",4,
                                              ifelse(Dolly_Buoy2$BUOY_ID == "pcnt2",5,
                                                     ifelse(Dolly_Buoy2$BUOY_ID == "ptit2",6,
                                                            ifelse(Dolly_Buoy2$BUOY_ID == "sdrt2",4,Dolly_Buoy2$BUOY_ID))))))


ggplot(data = Dolly_Buoy2) + geom_point(mapping=aes(x=date, y = wind), color = "red") +
  geom_point(mapping= aes(date, y = WSPD_adjusted), color =Dolly_Buoy2$BUOY_ID_F, shape= 10, size = 3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "top") + 
  ylab("Wind Speed") + xlab("Date")  + 
  labs(title = "Comparing Wind Speeds From Buoys and Hurr Tracks", subtitle= "Red = Hurr Tracks")
