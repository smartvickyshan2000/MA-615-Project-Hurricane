#Author: Carolyn Wright -- adapted from Haviland Wrights original code "buoy import-a"
#Date: 11/14/2021
#Abstract: This code pulls all buoy information relevant to Hurricane Dolly -2008
  #NOTE: Hurr_Tracks data has information collected for every 6 hours. Unclear 
  #      if it is a sum, average, or snapshot. Have created all equivalents below

#Import libraries
library(tidyverse)
library(magrittr)


#Create URLS for each Buoy that we will import for Hurricane Dolly 2008

  url_half1 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename="
  url_half2 <- "h2008.txt.gz&dir=data/historical/stdmet/"
  
  #Enter Buoy ID's near where Dolly made landfall in 2008
    #Notes:
      #Immediate zone:42045,PTIT2 ,RSJT2 ,42020 ,BABT2 ,IRDT2 ,MQTT2 ,PACT2 ,42002
      #Further south:42055
      #More north:RCPT2 ,SDRT2 ,PCNT2 ,42035
  
  buoys <- c('42020', '42045','42002','42055', '42035','ptit2', 'rsjt2', 'babt2', 
             'irdt2', 'mqtt2', 'pact2', 'rcpt2', 'sdrt2', 'pcnt2')
  
  #Create 14 urls
  urls <- str_c(url_half1, buoys, url_half2, sep = "")

#Pull in data and create overall buoy dataframe
  
  N <- length(urls) #N should equal the number of buoys used
  
  for (i in 1:N){
    #Read the data from the website
    suppressMessages(  ###  This stops the annoying messages on your screen.
      file <- read_table(urls[i], col_names = TRUE)
      )
    #Create Overall BUOYS dataframe. Subset down to dates in July of 2008
     if (i == 1){
       BUOYS_DF <- file %>% mutate(BUOY_ID = buoys[i]) %>% filter(MM != "mo" & MM == "07" )
       names(BUOYS_DF)[1]<-"YEAR"
     }
     #rbind all buoys together -- keep unique identifier for each buoy (BUOY_ID)
     if (i != 1){
       file %<>% mutate(BUOY_ID = buoys[i]) %>% filter(MM != "mo" & MM == "07")
       names(file)[1]<-"YEAR"
       BUOYS_DF <- rbind(BUOYS_DF,file )
     }
  }

#Get data in the correct form to cover Hurricane Dolly and merge onto Hurr_tracks data
  #Convert all character numerics to true numerics
    BUOYS_DF$HOUR = as.numeric(BUOYS_DF$hh)
    BUOYS_DF$DAY = as.numeric(BUOYS_DF$DD)
    BUOYS_DF$WDIR_NUM = as.numeric(BUOYS_DF$WDIR)
    BUOYS_DF$WSPD_NUM = as.numeric(BUOYS_DF$WSPD)
    BUOYS_DF$GST_NUM  = as.numeric(BUOYS_DF$GST)
    BUOYS_DF$WVHT_NUM = as.numeric(BUOYS_DF$WVHT)
    BUOYS_DF$DPD_NUM  = as.numeric(BUOYS_DF$DPD)
    BUOYS_DF$APD_NUM  = as.numeric(BUOYS_DF$APD)
    BUOYS_DF$MWD_NUM  = as.numeric(BUOYS_DF$MWD)
    BUOYS_DF$PRES_NUM = as.numeric(BUOYS_DF$PRES)
    BUOYS_DF$ATMP_NUM = as.numeric(BUOYS_DF$ATMP)
    BUOYS_DF$WTMP_NUM = as.numeric(BUOYS_DF$WTMP)
    BUOYS_DF$DEWP_NUM = as.numeric(BUOYS_DF$DEWP)
    BUOYS_DF$VIS_NUM  = as.numeric(BUOYS_DF$VIS)
    BUOYS_DF$TIDE_NUM = as.numeric(BUOYS_DF$TIDE)

  #Dolly took place between July 20 and July 27. Will subset down to those dates below;
    BUOYS_DF %<>% filter(DAY >19 & DAY <28)

    #Confirm we have the correct days
    table(BUOYS_DF$MM,BUOYS_DF$DAY)

  #NOTE: Hurr_Tracks data has information collected for every 6 hours. Unclear 
  #  if it is a sum. average, or snapshot. Have created all equivalents below


  #Create indicator of what quarter of the day the information was collected for -- 
  #  this will be used to sum up/take averages as needed
  BUOYS_DF$BUOYS_QTR <- ifelse(BUOYS_DF$HOUR<=6,1,
                               ifelse(BUOYS_DF$HOUR>6 & BUOYS_DF$HOUR<=12,2,
                                      ifelse(BUOYS_DF$HOUR>12 & BUOYS_DF$HOUR<=18,3,4)))
  
  #Create data drame with total and average values for each bouy by "quarter" of the day
  # BUOYS_6HOUR_SUMMARY <- BUOYS_DF %>% 
  #   group_by(BUOY_ID,MM,DD,BUOYS_QTR) %>% summarise(WDIR_SUM   = sum(WDIR_NUM),
  #                                                                     WSPD_SUM   = sum(WSPD_NUM),
  #                                                                     GST_SUM    = sum(GST_NUM), 
  #                                                                     WVHT_SUM   = sum(WVHT_NUM),
  #                                                                     DPD_SUM    = sum(DPD_NUM), 
  #                                                                     APD_SUM    = sum(APD_NUM), 
  #                                                                     MWD_SUM    = sum(MWD_NUM), 
  #                                                                     PRES_SUM   = sum(PRES_NUM),
  #                                                                     ATMP_SUM   = sum(ATMP_NUM),
  #                                                                     WTMP_SUM   = sum(WTMP_NUM),
  #                                                                     DEWP_SUM   = sum(DEWP_NUM),
  #                                                                     VIS_SUM    = sum(VIS_NUM), 
  #                                                                     TIDE_NUM   = sum(TIDE_NUM),
  #                                                                     WDIR_MEAN  = mean(WDIR_NUM),
  #                                                                     WSPD_MEAN  = mean(WSPD_NUM),
  #                                                                     GST_MEAN   = mean(GST_NUM), 
  #                                                                     WVHT_MEAN  = mean(WVHT_NUM),
  #                                                                     DPD_MEAN   = mean(DPD_NUM), 
  #                                                                     APD_MEAN   = mean(APD_NUM), 
  #                                                                     MWD_MEAN   = mean(MWD_NUM), 
  #                                                                     PRES_MEAN  = mean(PRES_NUM),
  #                                                                     ATMP_MEAN  = mean(ATMP_NUM),
  #                                                                     WTMP_MEAN  = mean(WTMP_NUM),
  #                                                                     DEWP_MEAN  = mean(DEWP_NUM),
  #                                                                     VIS_MEAN   = mean(VIS_NUM), 
  #                                                                     TIDE_MEAN  = mean(TIDE_NUM))
  
  #Subset down the full buoy data to just the 4 time points that are recorded in the hurr_tracks data
   # Remove buoys that follow a weird reporting schedule
    BUOYS_6HOUR_SNAPSHOT <- BUOYS_DF %>% filter(HOUR == 6 | HOUR == 12 | HOUR == 18 | HOUR == 00) %>% filter(BUOY_ID !="mqtt2" & BUOY_ID != "ptit2" & BUOY_ID != "rsjt2" & BUOY_ID != "42045")
    
    
    #Confirm we have the correct month, day,hour combination
    table(BUOYS_6HOUR_SNAPSHOT$MM,BUOYS_6HOUR_SNAPSHOT$DAY,BUOYS_6HOUR_SNAPSHOT$HOUR )
    
    table(BUOYS_6HOUR_SNAPSHOT$BUOY_ID)
    
    
    #Average wind speed per day/hour. Converted m/s to knots to match the hurricanes data
    #WSPD	Wind speed (m/s) averaged over an eight-minute period for buoys and a two-minute period 
    #for land stations. Reported Hourly. See Wind Averaging Methods.
    
    BUOYS_AVERAGE<- BUOYS_6HOUR_SNAPSHOT %>% mutate(WSPD_adjusted =WSPD_NUM*1.94 ) %>%
      group_by(MM,DD,hh) %>% summarise(WSPD_MEAN  = mean(WSPD_adjusted), WSPD_SD  = sd(WSPD_adjusted)) %>% 
      mutate(date =paste0("2008",MM,DD,hh,"00"))
    
    
    
