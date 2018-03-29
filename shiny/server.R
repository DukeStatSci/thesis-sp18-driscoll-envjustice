library(shinydashboard)
library(leaflet)
library(dplyr)
library(curl)
library(ggmap)  
library(rgdal)

#TODO: add a button (reaction button) observeEvent 
#TODO: figure out how to bind toxicity data to the spatial polygons
#TODO: add vertical line to the histogram indicating where in the dist you fall
#TODO: identify which polygon you are in the map
#TODO: pass tract data when zoom is > 11
#TODO: change the data being shown to only include immediate surrounding area. 
#TODO: make the map polygons clickable. 

counties = readOGR("C:/Users/Salena/Desktop/Anne/data/cb_2016_us_county_20m.shp", layer = "cb_2016_us_county_20m", GDAL1_integer64_policy = TRUE)
lower48 = subset(counties, !(counties$STATEFP %in% c("15", "02", "72")))
nc = subset(counties, counties$STATEFP %in% c("37"))
p = "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

function(input, output, session) {
  
  output$map = renderLeaflet({
    map = leaflet(nc) %>%
      addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5, opacity = 0.5, 
                  fillOpacity = 0.8, fillColor = ~colorQuantile("YlOrRd", ALAND)(ALAND),
                  highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE))
    
    latlon = geocode(input$addressInput, output = "latlon", source = "google")
    ll = SpatialPoints(latlon, proj4string = CRS(p))
    current = ll %over% nc
    
    map = map %>% setView(lng = latlon[1], lat = latlon[2], zoom = 11)
    
    map
  })
  
  output$hist = renderPlot({
   
    ggplot() + geom_line(aes(nc$ALAND), stat = "density") + xlab("Land Area") 
      #geom_segment(aes(x = current$ALAND, xend = current$ALAND, y = 0, yend = 1))
  }) 
}