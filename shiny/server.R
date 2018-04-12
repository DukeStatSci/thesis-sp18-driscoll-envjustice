library(shinydashboard)
library(leaflet)
library(dplyr)
library(curl)
library(ggmap)  
library(rgdal)
library(readr)
library(data.table)
library(stringr)
library(rmapshaper)

#TODO: make the map appear on startup
#TODO: create a warning thing for when geolocation fails.
#TODO: figure out how to change latlon as the map is shifted

data_county = fread("../index/data/toxic/toxic_2000_2010_county.csv")
data_tract = fread("../index/data/toxic/tract/toxic_2000_2010_tract.csv")
race_tract = fread("../index/data/census/race/race_tract_2000.csv")
#1000 on readin
names(data_county) = c("county", "tox")
names(data_tract) = c("tract", "tox", "area")
race_tract$id = str_pad(race_tract$id, 11, "left", pad = "0")

data_tract = merge(data_tract, race_tract, by.x = "tract", by.y = "id", all = TRUE) #300 ms
data_tract[is.na(data_tract$tox), "tox"] = 1*10^-6


states = readRDS("shapes/states.rds")
counties = readRDS("shapes/counties.rds")
counties = subset(counties, !(counties$STATEFP %in% c("15", "02", "72")))
counties@data = counties@data[, c(1, 5, 6, 8)]
counties@data$GEOID = as.character(counties@data$GEOID)
counties@data$STATEFP = as.character(counties@data$STATEFP)
counties@data =data.frame(counties@data, data_county[match(counties@data$GEOID, data_county$county), ])
counties@data$tox[is.na(counties@data$tox)] = 1*(10^-6)
#counties = ms_simplify(counties, keep_shapes)

matrix = read.csv("state_adjacent.csv", header = FALSE)
p = proj4string(counties)

find_adj_state = function(shape, states_list) {
  t = over(shape, states, returnList = TRUE)
  t = as.numeric(as.character(t[[1]]$STATEFP))
  #t = matrix[matrix$V1 %in% t, 2:9] #gets all the surrounding states
  #t = unique(unlist(t))
  t = as.character(t[!is.na(t)])
  t = t[!(t %in% states_list)]
  t = str_pad(t, 2, side = "left", pad = "0")
  t
}

pal = colorNumeric("magma", log(counties@data$tox) * abs(log(counties@data$tox)), na.color = "#C1C1C1", reverse = TRUE)

function(input, output, session) {
  
  values = reactiveValues(
    latlon = c(-78.94001, 36.00153), 
    current_c = SpatialPoints(matrix(c(-78.94001, 36.00153), nrow = 1), proj4string = CRS(p)) %over% counties,
    states_list = c("37", "51", "47", "13", "45")
  )
  
  data = reactiveValues(
    tract = data_tract[startsWith(as.character(data_tract$tract), as.character(isolate(values$current_c$GEOID))), ]
  )

  observeEvent(input$search, {
    values$latlon = geocode(input$addressInput, output = "latlon", source = "dsk")
    values$ll = SpatialPoints(matrix(as.numeric(values$latlon), nrow = 1), proj4string = CRS(p))
    values$current_c = values$ll %over% counties
    data$tract = data_tract[startsWith(as.character(data_tract$tract), as.character(values$current_c$GEOID)), ]
  })
  
  observeEvent(input$map_bounds, {
    shape = matrix(c(input$map_bounds$west, input$map_bounds$north, input$map_bounds$east, input$map_bounds$north, 
                     input$map_bounds$east, input$map_bounds$south, input$map_bounds$west, input$map_bounds$south), 
                   nrow = 4, byrow = TRUE)
    shape = SpatialPolygons(list(Polygons(list(Polygon(shape)), 1)))
    proj4string(shape) = p
    states_adj = find_adj_state(shape, values$states_list)
    values$states_list = append(values$states_list, states_adj)
    values$map_data = subset(counties, counties@data$STATEFP %in% states_adj)
    leafletProxy("map", session) %>%
      addPolygons(data = values$map_data, color = "#444444", weight = 1, smoothFactor = 0.5, 
                  opacity = 0.5, fillOpacity = 1, fillColor = ~pal(log(tox) * abs(log(tox)))
                 )
  })
  
  observeEvent(input$search, {
    leafletProxy("map", session) %>%
      setView(lng = values$latlon[1], lat = values$latlon[2], zoom = 9)
  })
  
  output$map = renderLeaflet({
    data = subset(counties, counties@data$STATEFP %in% c("37", "51", "47", "13", "45"))
    map = leaflet() %>%
      addPolygons(data = data, color = "#444444", weight = 1, smoothFactor = 0.5, 
                  opacity = 0.5, fillOpacity = 1, fillColor = ~pal(log(tox) * abs(log(tox)))
                  ) %>%
      addLegend(position = "topleft", title = "", pal = pal, values = log(counties@data$tox) * abs(log(counties@data$tox)))
    
    map = map %>% setView(lng = -78.94001, lat = 36.00153, zoom = 9) 
    
    map
  })
  
  output$national = renderPlot({
    validate(
      need(nrow(data$tract)>0, "No data for this area.")
    )
    comp = complete.cases(data$tract[, c(1:2, 4:11)])
    temp = data$tract[comp, ]
    
    state = substr(values$current_c$GEOID, 1, nchar(values$current_c$GEOID) - 3)
    state_tox = subset(counties, counties@data$STATEFP == state)@data$tox
    ggplot() + geom_vline(xintercept = values$current_c$tox, linetype = "longdash", alpha = 0.5) +
      geom_line(aes(counties@data$tox, color = "Nation"), stat = "density") + 
      geom_line(aes(state_tox, color = "State"), stat = "density") +
      geom_line(aes(temp$tox, color = "Within County"), stat = "density") +
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
    validate(
      need(nrow(data$tract)>0, "No data for this area."),
      need(!(max(data$tract$tox) == min(data$tract$tox) && min(data$tract$tox) == 1*(10^-6)), "There are no spills recorded in your area!")
    )
    comp = complete.cases(data$tract[, c(1:2, 4:11)])
    temp = data$tract[comp, ]
    ggplot() + geom_density(aes(temp$tox, weight = temp$black/sum(temp$black), fill = "black"), alpha = 0.4 , color = NA) + xlab("Log Toxicity") +
      geom_density(aes(temp$tox, weight = temp$white/sum(temp$white), fill = "white"), alpha = 0.4 , color = NA) +
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