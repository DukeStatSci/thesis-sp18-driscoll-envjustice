t1990 = as.data.frame(fread("index/datatoxic/tract/toxic_1990_2010_tract.csv", data.table = FALSE))
t1991 = as.data.frame(fread("index/datatoxic/tract/toxic_1991_2010_tract.csv", data.table = FALSE))
t1992 = as.data.frame(fread("index/datatoxic/tract/toxic_1992_2010_tract.csv", data.table = FALSE))
t1993 = as.data.frame(fread("index/datatoxic/tract/toxic_1993_2010_tract.csv", data.table = FALSE))
t1994 = as.data.frame(fread("index/datatoxic/tract/toxic_1994_2010_tract.csv", data.table = FALSE))
#t1995 = as.data.frame(fread("index/datatoxic/tract/toxic_1995_2010_tract.csv", data.table = FALSE))
t1996 = as.data.frame(fread("index/datatoxic/tract/toxic_1996_2010_tract.csv", data.table = FALSE))
t1997 = as.data.frame(fread("index/datatoxic/tract/toxic_1997_2010_tract.csv", data.table = FALSE))
t1998 = as.data.frame(fread("index/datatoxic/tract/toxic_1998_2010_tract.csv", data.table = FALSE))
t1999 = as.data.frame(fread("index/datatoxic/tract/toxic_1999_2010_tract.csv", data.table = FALSE))
t2000 = as.data.frame(fread("index/datatoxic/tract/toxic_2000_2010_tract.csv", data.table = FALSE))
t2001 = as.data.frame(fread("index/datatoxic/tract/toxic_2001_2010_tract.csv", data.table = FALSE))
t2002 = as.data.frame(fread("index/datatoxic/tract/toxic_2002_2010_tract.csv", data.table = FALSE))
t2003 = as.data.frame(fread("index/datatoxic/tract/toxic_2003_2010_tract.csv", data.table = FALSE))
#t2004 = as.data.frame(fread("index/datatoxic/tract/toxic_2004_2010_tract.csv", data.table = FALSE))
t2005 = as.data.frame(fread("index/datatoxic/tract/toxic_2005_2010_tract.csv", data.table = FALSE))
t2006 = as.data.frame(fread("index/datatoxic/tract/toxic_2006_2010_tract.csv", data.table = FALSE))
t2007 = as.data.frame(fread("index/datatoxic/tract/toxic_2007_2010_tract.csv", data.table = FALSE))
t2008 = as.data.frame(fread("index/datatoxic/tract/toxic_2008_2010_tract.csv", data.table = FALSE))
t2009 = as.data.frame(fread("index/datatoxic/tract/toxic_2009_2010_tract.csv", data.table = FALSE))
t2010 = as.data.frame(fread("index/datatoxic/tract/toxic_2010_2010_tract.csv", data.table = FALSE))
t2011 = as.data.frame(fread("index/datatoxic/tract/toxic_2011_2010_tract.csv", data.table = FALSE))
t2012 = as.data.frame(fread("index/datatoxic/tract/toxic_2012_2010_tract.csv", data.table = FALSE))
t2013 = as.data.frame(fread("index/datatoxic/tract/toxic_2013_2010_tract.csv", data.table = FALSE))
t2014 = as.data.frame(fread("index/datatoxic/tract/toxic_2014_2010_tract.csv", data.table = FALSE))

read_in_pov = function(year, tox) {
  data = as.data.frame(fread(paste0("index/datacensus/hisp_pov/hisp_pov_tract_", year, ".csv"), data.table = FALSE))
  data = data[complete.cases(data), ]
  data$id = str_pad(data$id, 11, pad = "0", side = "left")
  data = merge(data, tox, by.x = "id", by.y = "block")
  data$year = year
  return(data)
}

p1990 = read_in_pov(1990, t1990)
p1991 = read_in_pov(1991, t1991)
p1992 = read_in_pov(1992, t1992)
p1993 = read_in_pov(1993, t1993)
p1994 = read_in_pov(1994, t1994)
p1996 = read_in_pov(1996, t1996)
#p1995 = read_in_pov(1995, t1995)
p1997 = read_in_pov(1997, t1997)
p1998 = read_in_pov(1998, t1998)
p1999 = read_in_pov(1999, t1999)
p2000 = read_in_pov(2000, t2000)
p2001 = read_in_pov(2001, t2001)
p2002 = read_in_pov(2002, t2002)
p2003 = read_in_pov(2003, t2003)
#p2004 = read_in_pov(2004, t2004)
p2005 = read_in_pov(2005, t2005)
p2006 = read_in_pov(2006, t2006)
p2007 = read_in_pov(2007, t2007)
p2008 = read_in_pov(2008, t2008)
p2009 = read_in_pov(2009, t2009)
p2010 = read_in_pov(2010, t2010)
p2011 = read_in_pov(2011, t2011)
p2012 = read_in_pov(2012, t2012)
p2013 = read_in_pov(2013, t2013)

pov_data = rbind.fill(p1990, p1991, p1992, p1993, p1994, p1996, p1997, p1998, p1999, p2000, p2001, p2002, p2003, p2005, p2006, p2007, p2008, p2009, p2010, p2011, p2012, p2013)

#Environmental justice literature has focused largely on the motivations behind facility siting that lead to inequality and policy approaches to help fix the disproportionate burden of environmental hazards on minority communities. The questions being asked are critical to creating policy that helps correct the problem of hazard allocation. A gap in the literature is the need for an investigation of the dynamic world of hazard allocation over time. 

# what is natural sorting, How are env dumps created?, Breaking the self fulfilling cycle

read_in = function(year, tox) {
  data = as.data.frame(fread(paste0("index/datacensus/hisp/hisp_tract_", year, ".csv"), data.table = FALSE))
  data = data[complete.cases(data), ]
  data$other_sum = rowSums(data[, 5:7])
  data = select(data, id, pop, white, black, hispanic, other_sum)
  names(data) = c("id", "pop", "white", "black", "hispanic", "other")
  data$id = str_pad(data$id, 11, pad = "0", side = "left")
  data = merge(data, tox, by.x = "id", by.y = "block")
  data$year = year
  return(data)
}

h1990 = read_in(1990, t1990)
h1991 = read_in(1991, t1991)
h1992 = read_in(1992, t1992)
h1993 = read_in(1993, t1993)
h1994 = read_in(1994, t1994)
#h1995 = read_in(1995, t1995)
h1996 = read_in(1996, t1996)
h1997 = read_in(1997, t1997)
h1998 = read_in(1998, t1998)
h1999 = read_in(1999, t1999)
h2000 = read_in(2000, t2000)
h2001 = read_in(2001, t2001)
h2002 = read_in(2002, t2002)
h2003 = read_in(2003, t2003)
#h2004 = read_in(2004, t2004)
h2005 = read_in(2005, t2005)
h2006 = read_in(2006, t2006)
h2007 = read_in(2007, t2007)
h2008 = read_in(2008, t2008)
h2009 = read_in(2009, t2009)
h2010 = read_in(2010, t2010)
h2011 = read_in(2011, t2011)
h2012 = read_in(2012, t2012)
h2013 = read_in(2013, t2013)

hisp_data = rbind.fill(h1990, h1991, h1992, h1993, h1994, h1996, h1997, h1998, h1999, h2000, h2001, h2002, h2003, h2005, h2006, h2007, h2008, h2009, h2010, h2011, h2012, h2013)


get_quants = function(data, col = "black", log = NA, n = 20000) { 
  years = unique(data$year)
  output = data.frame(matrix(0, ncol = length(years), nrow = n))
  tox = "concentration"
  if (!is.na(log)){
    tox = "lconcentration"
    data$lconcentration = log(data$concentration)
  }
  for (i in 1:length(years)) {
    year = years[i]
    #sample toxicities with probability given by race col
    samp = sample(data[data$year == year, tox], n, replace = TRUE, prob = data[data$year == year, col])
    density = ewcdf(data[data$year == year, tox], weights = (data[data$year == year, ]$pop)/sum(data[data$year == year, ]$pop, na.rm = TRUE))
    perc = density(samp)
    output[, i] = perc
  }
  names(output) = paste0(col, years)
  return(output)
}

black_quant = get_quants(hisp_data)
white_quant = get_quants(hisp_data, "white")
other_quant = get_quants(hisp_data, "other")
hisp_quant = get_quants(hisp_data, "hispanic")

quants = cbind(black_quant, white_quant, other_quant, hisp_quant)
write.csv(quants, "index/data/quantiles.csv", row.names = FALSE)