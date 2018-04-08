mapC = map("county", fill = TRUE, plot = FALSE, resolution = 0)
data("county.fips")
counties = county.fips %>% left_join(data_county, by=c('fips'='county'))
counties[is.na(counties$tox), "tox"]  = 0.0001
counties$color <- gray(abs(log(counties$tox)) / max(log(counties$tox), na.rm = TRUE))
mapC$tox = counties$tox

leaflet(data = mapC)  %>%
  addPolygons(fillColor = tox, stroke = FALSE)

map("county", fill=TRUE, col=counties$color)