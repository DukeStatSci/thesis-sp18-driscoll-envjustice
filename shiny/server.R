library(shinydashboard)
library(leaflet)
library(dplyr)
library(curl)
library(ggmap)  
library(rgdal)
library(readr)

#TODO: figure out why the 01 states don't bind data
#TODO: pass tract data when zoom is > 11
#TODO: change the data being show n to only include immediate surrounding area. 
#TODO: make the map polygons clickable. 
#TODO: make the map appear on startup
#TODO: create a warning thing for when geolocation fails.

data_county = read.csv("../index/data/toxic/toxic_2000_2010_county.csv")
data_tract = read.csv("../index/data/toxic/tract/toxic_2000_2010_tract.csv")
race_tract = read.csv("../index/data/census/race/race_tract_2000.csv")
names(data_county) = c("county", "tox")
names(data_tract) = c("tract", "tox", "area")

data_tract = merge(data_tract, race_tract, by.x = "tract", by.y = "id")

counties = readOGR("cb_2016_us_county_20m.shp", layer = "cb_2016_us_county_20m", GDAL1_integer64_policy = TRUE)
tracts = readOGR("cb_2016_us_county_20m.shp", layer = "cb_2016_us_county_20m", GDAL1_integer64_policy = TRUE)
counties@data = counties@data[, c(1, 5, 6, 8)]
counties@data =data.frame(counties@data, data_county[match(as.numeric(as.character(counties@data[,"GEOID"])), data_county[,"county"]),])

#lower48 = subset(counties, !(counties$STATEFP %in% c("15", "02", "72")))
nc = subset(counties, counties$STATEFP == "37")
p = "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

pal = colorNumeric("magma", log(counties@data$tox), na.color = "#C1C1C1", reverse = TRUE)

function(input, output, session) {
  
  #get the lat/long reactively upon the user searching
  latlon = eventReactive(input$search, {geocode(input$addressInput, output = "latlon", source = "dsk")})
  
  #create the SpatialPoint based on lat/long
  ll = reactive({SpatialPoints(latlon(), proj4string = CRS(p))})
  
  #get the Spatial Polygon that coresponds to the correct location
  current_c = reactive({ll() %over% counties})
  current_t = reactive({ll() %over% tracts})
  
  tract = reactive({data_tract[startsWith(as.character(data_tract$tract), as.character(current_c()$GEOID)), ]})
  
  output$map = renderLeaflet({
    map = leaflet(nc) %>%
      addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5, opacity = 0.5, 
                  fillOpacity = 0.8, fillColor = ~pal(log(tox)),
                  highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE))
    
    map = map %>% setView(lng = latlon()[1], lat = latlon()[2], zoom = 11) 
    
    map
  })
  
  output$national = renderPlot({
    ggplot() + geom_vline(xintercept = current_c()$tox, linetype = "longdash", alpha = 0.5) +
      geom_line(aes(nc$tox, color = "National"), stat = "density") + xlab("Log Toxicity") +
      geom_line(aes(tract()$tox, color = "Within County"), stat = "density") +
      theme(legend.position = "bottom") + 
      scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
        labels = scales::trans_format("log10", scales::math_format(10^.x))) +
      annotation_logticks(
        short = unit(.5,"mm"),
        mid = unit(2,"mm"), 
        long = unit(3.5,"mm"),
        sides = "l",
        color = "gray65") 
  }) 
  
  output$race = renderPlot({
    ggplot() + geom_density(aes(tract()$tox, weight = tract()$black/sum(tract()$black), color = "black")) + xlab("Log Toxicity") +
      geom_density(aes(tract()$tox, weight = tract()$white/sum(tract()$white), color = "white")) +
      theme(legend.position = "bottom") + 
      scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                    labels = scales::trans_format("log10", scales::math_format(10^.x))) +
      annotation_logticks(
        short = unit(.5,"mm"),
        mid = unit(2,"mm"), 
        long = unit(3.5,"mm"),
        sides = "b",
        color = "gray65") 
  })
}