library(shinydashboard)
library(leaflet)
library(dplyr)
library(curl)
library(ggmap)  
library(rgdal)
library(readr)
library(data.table)
library(feather)
library(stringr)
library(rmapshaper)

#TODO: make the map appear on startup
#TODO: create a warning thing for when geolocation fails.
#TODO: figure out how to change latlon as the map is shifted

data_tract = as.data.table(read_feather("shapes/data_tract_race.feather"))
states = readRDS("shapes/states.rds")
counties = readRDS("shapes/counties_data.rds")
matrix = read.csv("state_adjacent.csv", header = FALSE)
p = proj4string(counties)

find_adj_state = function(shape, states_list) {
  t = over(shape, states, returnList = TRUE)
  t = as.numeric(as.character(t[[1]]$STATEFP))
  t = as.character(t[!is.na(t)])
  t = t[!(t %in% states_list)]
  t = str_pad(t, 2, side = "left", pad = "0")
  t
}

convert_to_color = function(data) {
  data = data^.1
  (data - mean((counties@data$tox)^.1))/sd(counties@data$tox^.1)
}
get_county = function(latlon) {
  current_s = as.character((latlon %over% states)$STATEFP)
  temp_c = counties[startsWith(as.character(counties@data$GEOID), current_s), ]
  latlon %over% temp_c
}

pal = colorNumeric("magma", convert_to_color(counties@data$tox), na.color = "#C1C1C1", reverse = TRUE)

function(input, output, session) {
  
  values = reactiveValues(
    out = FALSE,
    latlon = c(-78.94001, 36.00153), 
    current_c = get_county(SpatialPoints(matrix(c(-78.94001, 36.00153), nrow = 1), proj4string = CRS(p))),
    states_list = c("37", "51", "47", "13", "45")
  )
  
  data = reactiveValues(
    tract = data_tract[startsWith(as.character(data_tract$tract), as.character(isolate(values$current_c$GEOID))), ]
  )
  
  outside_area = reactive(values$out)

  output$warning = renderText({
    if (outside_area()){
      values$out = FALSE
      "This region is not supported."
    }
    else{
      ""
    }
  
  })
  
  observeEvent(input$search, {
    values$latlon = geocode(input$addressInput, output = "latlon", source = "dsk")
    #validate location found
    if (!is.na(values$latlon)[1]) {
      values$ll = SpatialPoints(matrix(as.numeric(values$latlon), nrow = 1), proj4string = CRS(p))
      #validate in us
      if (!is.na((values$ll %over% states)[1])) {
        values$out = FALSE
        values$current_c = get_county(values$ll)
        data$tract = data_tract[startsWith(as.character(data_tract$tract), as.character(values$current_c$GEOID)), ]
        leafletProxy("map", session) %>%
          setView(lng = values$latlon[1], lat = values$latlon[2], zoom = 9)
      }
    }
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
      addPolygons(data = values$map_data, color = "#444444", weight = 0.5, smoothFactor = 0.5, 
                  opacity = 1, fillOpacity = 1, fillColor = ~pal(convert_to_color(tox))
                 ) %>%
      addPolygons(data = states, color = "#333333", weight = 1, smoothFactor = 0.5,
                              opacity = 1, fill = FALSE)
  })
  
  output$map = renderLeaflet({
    data = subset(counties, counties@data$STATEFP %in% c("37", "51", "47", "13", "45"))
    map = leaflet() %>%
      addPolygons(data = data, color = "#444444", weight = 0.5, smoothFactor = 0.5, 
                  opacity = 1, fillOpacity = 1, fillColor = ~pal(convert_to_color(tox))
                  ) %>%
      addPolygons(data = states, color = "#333333", weight = 1, smoothFactor = 0.5,
                  opacity = 1, fill = FALSE) %>%
      addLegend(position = "topleft", title = "", pal = pal, values = convert_to_color(counties@data$tox))
    
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
                    limits = c(.01, 160000)) +
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
      need(!(max(data$tract$tox) == min(data$tract$tox)), "There are no spills recorded in your area!")
    )
    comp = complete.cases(data$tract[, c(1:2, 4:11)])
    temp = data$tract[comp, ]
    head(temp)
    ggplot() + geom_density(aes(temp$tox, weight = temp$black/sum(temp$black), fill = "black"), alpha = 0.4 , color = NA) + xlab("Log Toxicity") +
      geom_density(aes(temp$tox, weight = temp$white/sum(temp$white), fill = "white"), alpha = 0.4 , color = NA) +
      theme_classic() +
      theme(legend.position = "bottom", axis.line.y = element_line(color = "grey"), axis.line.x = element_line(color = "grey")) +
      scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                    labels = scales::trans_format("log10", scales::math_format(10^.x)),
                    limits = c(.01, 160000)) +
      annotation_logticks(
        short = unit(.5,"mm"),
        mid = unit(2,"mm"), 
        long = unit(3.5,"mm"),
        sides = "b",
        color = "gray65")
  })
}