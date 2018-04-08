library(shinydashboard)
library(leaflet)
library(dplyr)
library(curl)
library(ggmap)  
library(rgdal)
library(readr)

#TODO: profiling shiny apps
#TODO: pass tract data when zoom is > 11
#TODO: make the map appear on startup
#TODO: create a warning thing for when geolocation fails.
#TODO: figure out how to change latlon as the map is shifted

data_county = read.csv("../index/data/toxic/toxic_2000_2010_county.csv")
data_tract = read.csv("../index/data/toxic/tract/toxic_2000_2010_tract.csv")
race_tract = read.csv("../index/data/census/race/race_tract_2000.csv")
#1000 on readin
names(data_county) = c("county", "tox")
names(data_tract) = c("tract", "tox", "area")

data_tract = merge(data_tract, race_tract, by.x = "tract", by.y = "id") #300 ms

counties = readOGR("cb_2016_us_county_20m.shp", layer = "cb_2016_us_county_20m", GDAL1_integer64_policy = TRUE) #3000ms
counties@data = counties@data[, c(1, 5, 6, 8)]
counties@data$GEOID = as.numeric(as.character(counties@data$GEOID))
counties@data$STATEFP = as.numeric(as.character(counties@data$STATEFP))
counties@data =data.frame(counties@data, data_county[match(counties@data[,"GEOID"], data_county[,"county"]),])

#lower48 = subset(counties, !(counties$STATEFP %in% c("15", "02", "72")))
matrix = read.csv("state_adjacent.csv", header = FALSE)
nc = subset(counties, counties$STATEFP == "37")
p = "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"


pal = colorNumeric("magma", log(counties@data$tox) * abs(log(counties@data$tox)), na.color = "#C1C1C1", reverse = TRUE)

function(input, output, session) {
  
  #get the lat/long reactively upon the user searching
  latlon = eventReactive(input$search, {geocode(input$addressInput, output = "latlon", source = "dsk")})
  
  #create the SpatialPoint based on lat/long
  ll = reactive({SpatialPoints(latlon(), proj4string = CRS(p))})
  
  #get the Spatial Polygon that coresponds to the correct location
  current_c = reactive({ll() %over% counties})
  
  tract = reactive({data_tract[startsWith(as.character(data_tract$tract), as.character(current_c()$GEOID)), ]})
 
  adjacent_list = reactive({ 
    unlist(matrix[matrix$V1 == current_c()$STATEFP, ], use.names = FALSE)[!is.na(unlist(matrix[matrix$V1 == current_c()$STATEFP, ], use.names = FALSE))]
  }) #weirdly slow 500ms
   
  map_data = reactive({ 
    #print(adjacent_list())
    subset(counties, counties@data$STATEFP %in% adjacent_list())
  })
  
  output$map = renderLeaflet({
    map = leaflet(map_data()) %>%
      addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5, opacity = 0.5, 
                  fillOpacity = 0.8, fillColor = ~pal(log(tox) * abs(log(tox)))
                  #, highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)
                  ) %>%
      addLegend(position = "topleft", title = "", pal = pal, values = log(counties@data$tox) * abs(log(counties@data$tox)))
    
    map = map %>% setView(lng = latlon()[1], lat = latlon()[2], zoom = 9) 
    
    map
  })
  
  output$national = renderPlot({
    state = as.numeric(substr(current_c()$GEOID, 1, nchar(current_c()$GEOID) - 3))
    state_tox = subset(counties, counties@data$STATEFP == state)@data$tox
    ggplot() + geom_vline(xintercept = current_c()$tox, linetype = "longdash", alpha = 0.5) +
      geom_line(aes(counties$tox, color = "Nation"), stat = "density") + 
      geom_line(aes(state_tox, color = "State"), stat = "density") +
      geom_line(aes(tract()$tox, color = "Within County"), stat = "density") +
      theme_classic() +
      theme(legend.position = "bottom", axis.line.y = element_line(color = "grey"), axis.line.x = element_line(color = "grey")) + 
      xlab("Log Toxicity") +
      scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                    labels = scales::trans_format("log10", scales::math_format(10^.x)),
                    limits = c(1, 160000)) +
      annotation_logticks(
        short = unit(.5,"mm"),
        mid = unit(2,"mm"), 
        long = unit(3.5,"mm"),
        sides = "b",
        color = "gray65")
  }) 
  
  output$race = renderPlot({
    ggplot() + geom_density(aes(tract()$tox, weight = tract()$black/sum(tract()$black), fill = "black"), alpha = 0.4 , color = NA) + xlab("Log Toxicity") +
      geom_density(aes(tract()$tox, weight = tract()$white/sum(tract()$white), fill = "white"), alpha = 0.4 , color = NA) +
      theme_classic() +
      theme(legend.position = "bottom", axis.line.y = element_line(color = "grey"), axis.line.x = element_line(color = "grey")) +
      scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                    labels = scales::trans_format("log10", scales::math_format(10^.x)),
                    limits = c(1, 160000)) +
      annotation_logticks(
        short = unit(.5,"mm"),
        mid = unit(2,"mm"), 
        long = unit(3.5,"mm"),
        sides = "b",
        color = "gray65")
  })
}