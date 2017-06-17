# examples from Peers

library(leaflet)
mymap <- leaflet() %>% addTiles() 

## Point the location of UCSD biomedical library
mymap <- mymap %>% addMarkers(lat=32.875541, lng= -117.236895, popup="University of California San Diego Biomedical Library")
mymap

#################################

library(leaflet)
earthquakes <- data.frame(time = c("6:04:39 pm", "5:36:31 pm", "4:54:56 pm", "2:20:36 pm", "1:04:41 pm", "12:04:46 am", "10:36:14 am", "9:58:45 am"),
                          magnitude = c(4.1, 2.3, 5.6, 4.7, 3.8, 6.0, 4.1, 3.3), 
                          lat = c(-40.70, -43.73, -41.70, -34.02, -38.89, -45.60, -43.56, -42.30),
                          lon = c(175.16, 170.18, 174.16, 174.45, 175.79, 167.88, 169.74, 173.28))

earthquakes %>% 
        leaflet() %>%
        addTiles() %>%
        addCircles(weight = 1, radius = earthquakes$magnitude * earthquakes$magnitude * 4000) %>%
        addMarkers(earthquakes$lon, earthquakes$lat, popup = earthquakes$time, label = earthquakes$time)



#################################

m <- leaflet() %>%
        addTiles() %>%  
        addMarkers(lng=37.620407, lat=55.754093, popup="RedSquare")
m  # Print the map
