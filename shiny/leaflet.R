library(rgdal)

# From census sbf files, 20m
counties = readOGR("C:/Users/Salena/Desktop/Anne/data/cb_2016_us_county_20m.shp", layer = "cb_2016_us_county_20m", GDAL1_integer64_policy = TRUE)

lower48 = subset(counties, !(counties$STATEFP %in% c("15", "02", "72")))
nc = subset(counties, counties$STATEFP %in% c("37"))

leaflet(nc) %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5, opacity = 0.5, 
              fillOpacity = 0.8, fillColor = ~colorQuantile("YlOrRd", ALAND)(ALAND),
              highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE))


#adjacency matrix adapted from http://www.biostat.umn.edu/~brad/data/contig-lower48.dat to use census numbers

proj = "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
ll = SpatialPoints(c(latlon[1], latlon[2]), proj4string = CRS(proj))

