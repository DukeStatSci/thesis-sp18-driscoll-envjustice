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

data_county = fread("../index/data/toxic/toxic_2000_2010_county.csv")
data_tract = fread("../index/data/toxic/tract/toxic_2000_2010_tract.csv")
race_tract = fread("../index/data/census/race/race_tract_2000.csv")
#1000 on readin
names(data_county) = c("county", "tox")
names(data_tract) = c("tract", "tox", "area")
race_tract$id = str_pad(race_tract$id, 11, "left", pad = "0")

data_tract = merge(data_tract, race_tract, by.x = "tract", by.y = "id", all = TRUE) #300 ms
data_tract[is.na(data_tract$tox), "tox"] = 1*10^-6

counties = readOGR("cb_2016_us_county_20m.shp", layer = "cb_2016_us_county_20m", GDAL1_integer64_policy = TRUE) #2000ms
counties = subset(counties, !(counties$STATEFP %in% c("15", "02", "72")))
counties@data = counties@data[, c(1, 5, 6, 8)]
counties@data$GEOID = as.character(counties@data$GEOID)
counties@data$STATEFP = as.character(counties@data$STATEFP)
counties@data =data.frame(counties@data, data_county[match(counties@data$GEOID, data_county$county), ])
counties@data$tox[is.na(counties@data$tox)] = 1*(10^-6)
counties = ms_simplify(counties, keep = 0.25, keep_shapes = TRUE)


counties20@data = counties20@data[, c(1,2,6)]
td <- file.path("C:/Users/Salena/Documents/thesis-sp18-driscoll-envjustice/shiny", "counties"); dir.create(td)
writeOGR(counties20, td, "counties20", driver="ESRI Shapefile")
