library(plotly)
library(tidyverse)



Dolly_Buoy2 %>% 
  plot_ly() %>%
  add_trace(x=~longitude,y=~latitude,z=~wind,type="mesh3d") %>%
  add_trace(x = ~long2, y = ~lat, z = ~WSPD_adjusted, mode = "markers", type = "scatter3d", 
            marker = list(size = 5, color = "red", symbol = 104))


Dolly_rain_precip %>%
  plot_ly() %>%
  add_trace(x=~long,y=~lat,z=~rainfall,type="mesh3d")
