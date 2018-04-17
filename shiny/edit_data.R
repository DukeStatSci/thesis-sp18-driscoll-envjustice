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
library(plyr)
library(rseiAnalysis)

data_tract = as.data.table(read_csv("shapes/toxic_2010_2010_tract.csv"))
data_county = aggregateGeography(data_tract, "county")
race_tract = as.data.table(read_feather("shapes/race_tract.feather"))

names(data_county) = c("county", "tox", "area")
names(data_tract) = c("tract", "tox", "area")
race_tract$id = str_pad(race_tract$id, 11, "left", pad = "0")

data_tract = merge(data_tract, race_tract, by.x = "tract", by.y = "id", all = TRUE) #300 ms
data_tract[is.na(data_tract$tox), "tox"] = min(data_tract$tox, na.rm = TRUE) - (min(data_tract$tox, na.rm = TRUE)*0.01)


states = readOGR("shapes/cb_2017_us_state_20m.shp", "cb_2017_us_state_20m")
states = subset(states, !(states$STATEFP %in% c("15", "02", "72")))
#counties = readRDS("shapes/counties.rds")
counties = readOGR("shapes/cb_2016_us_county_20m.shp", "cb_2016_us_county_20m")
counties = subset(counties, !(counties$STATEFP %in% c("15", "02", "72")))
counties@data = counties@data[, c(1, 5, 6, 8)]
counties@data$GEOID = as.character(counties@data$GEOID)
counties@data$STATEFP = as.character(counties@data$STATEFP)
counties@data = data.frame(counties@data, data_county[match(counties@data$GEOID, data_county$county), ])
counties@data$tox[is.na(counties@data$tox)] = min(data_county$tox) - (min(data_county$tox)*0.01)
#counties = ms_simplify(counties, keep_shapes)

write_rds(counties, "shapes/counties_data.rds")
write_rds(states, "shapes/states.rds")
write_feather(data_tract, "shapes/data_tract_race.feather")