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

data_county = read_feather("shapes/data_county.feather")
data_tract = as.data.table(read_feather("shapes/data_tract.feather"))
race_tract = as.data.table(read_feather("shapes/race_tract.feather"))
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
counties@data = data.frame(counties@data, data_county[match(counties@data$GEOID, data_county$county), ])
counties@data$tox[is.na(counties@data$tox)] = 1*(10^-6)
#counties = ms_simplify(counties, keep_shapes)

write_rds(counties, "counties_data.rds")



data_tract = as.data.table(read_feather("shapes/data_tract_race.feather"))
race_tract = as.data.table(read_feather("shapes/race_tract.feather"))
#1000 on readin
names(data_county) = c("county", "tox")
names(data_tract) = c("tract", "tox", "area")
race_tract$id = str_pad(race_tract$id, 11, "left", pad = "0")

data_tract = merge(data_tract, race_tract, by.x = "tract", by.y = "id", all = TRUE) #300 ms
data_tract[is.na(data_tract$tox), "tox"] = 1*10^-6
write_feather(data_tract, "data_tract_race.feather")