library(readr)
library(dplyr)
library(plyr)
library(data.table)
devtools::install_github("amd112/rseiAnalysis")
library("rseiAnalysis")
#needed for RSEI
set.seed(1294045)

#read in toxicity data and race data for all years
t1990 = as.data.frame(fread("data/toxic/tract/toxic_1990_2010_tract.csv", data.table = FALSE))
t1991 = as.data.frame(fread("data/toxic/tract/toxic_1991_2010_tract.csv", data.table = FALSE))
t1992 = as.data.frame(fread("data/toxic/tract/toxic_1992_2010_tract.csv", data.table = FALSE))
t1993 = as.data.frame(fread("data/toxic/tract/toxic_1993_2010_tract.csv", data.table = FALSE))
t1994 = as.data.frame(fread("data/toxic/tract/toxic_1994_2010_tract.csv", data.table = FALSE))
#t1995 = as.data.frame(fread("data/toxic/tract/toxic_1995_2010_tract.csv", data.table = FALSE))
t1996 = as.data.frame(fread("data/toxic/tract/toxic_1996_2010_tract.csv", data.table = FALSE))
t1997 = as.data.frame(fread("data/toxic/tract/toxic_1997_2010_tract.csv", data.table = FALSE))
t1998 = as.data.frame(fread("data/toxic/tract/toxic_1998_2010_tract.csv", data.table = FALSE))
t1999 = as.data.frame(fread("data/toxic/tract/toxic_1999_2010_tract.csv", data.table = FALSE))
t2000 = as.data.frame(fread("data/toxic/tract/toxic_2000_2010_tract.csv", data.table = FALSE))
t2001 = as.data.frame(fread("data/toxic/tract/toxic_2001_2010_tract.csv", data.table = FALSE))
t2002 = as.data.frame(fread("data/toxic/tract/toxic_2002_2010_tract.csv", data.table = FALSE))
t2003 = as.data.frame(fread("data/toxic/tract/toxic_2003_2010_tract.csv", data.table = FALSE))
#t2004 = as.data.frame(fread("data/toxic/tract/toxic_2004_2010_tract.csv", data.table = FALSE))
t2005 = as.data.frame(fread("data/toxic/tract/toxic_2005_2010_tract.csv", data.table = FALSE))
t2006 = as.data.frame(fread("data/toxic/tract/toxic_2006_2010_tract.csv", data.table = FALSE))
t2007 = as.data.frame(fread("data/toxic/tract/toxic_2007_2010_tract.csv", data.table = FALSE))
t2008 = as.data.frame(fread("data/toxic/tract/toxic_2008_2010_tract.csv", data.table = FALSE))
t2009 = as.data.frame(fread("data/toxic/tract/toxic_2009_2010_tract.csv", data.table = FALSE))
t2010 = as.data.frame(fread("data/toxic/tract/toxic_2010_2010_tract.csv", data.table = FALSE))
t2011 = as.data.frame(fread("data/toxic/tract/toxic_2011_2010_tract.csv", data.table = FALSE))
t2012 = as.data.frame(fread("data/toxic/tract/toxic_2012_2010_tract.csv", data.table = FALSE))
t2013 = as.data.frame(fread("data/toxic/tract/toxic_2013_2010_tract.csv", data.table = FALSE))
t2014 = as.data.frame(fread("data/toxic/tract/toxic_2014_2010_tract.csv", data.table = FALSE))

r1990 = as.data.frame(fread("data/census/race_tract_1990.csv", data.table = FALSE))
r1990 = r1990[, c(7, 11:ncol(r1990))]
names(r1990) = c("id", "pop", "white", "black", "native", "asian", "other")
r1990 = merge(r1990, t1990, by.x = "id", by.y = "block")
r1990$lconcentration = log(r1990$concentration)
r1990$year = 1990
r1991 = as.data.frame(fread("data/census/race_tract_1991.csv", data.table = FALSE))
r1991 = na.omit(merge(r1991, t1991, by.x = "id", by.y = "block"))
r1991$lconcentration = log(r1991$concentration)
r1991$year = 1991
r1992 = as.data.frame(fread("data/census/race_tract_1992.csv", data.table = FALSE))
r1992 = na.omit(merge(r1992, t1992, by.x = "id", by.y = "block"))
r1992$lconcentration = log(r1992$concentration)
r1992$year = 1992
r1993 = as.data.frame(fread("data/census/race_tract_1993.csv", data.table = FALSE))
r1993 = na.omit(merge(r1993, t1993, by.x = "id", by.y = "block"))
r1993$lconcentration = log(r1993$concentration)
r1993$year = 1993
r1994 = as.data.frame(fread("data/census/race_tract_1994.csv", data.table = FALSE))
r1994 = na.omit(merge(r1994, t1994, by.x = "id", by.y = "block"))
r1994$lconcentration = log(r1994$concentration)
r1994$year = 1994
#r1995 = as.data.frame(fread("data/census/race_tract_1995.csv", data.table = FALSE))
r1996 = as.data.frame(fread("data/census/race_tract_1996.csv", data.table = FALSE))
r1996 = na.omit(merge(r1996, t1996, by.x = "id", by.y = "block"))
r1996$lconcentration = log(r1996$concentration)
r1996$year = 1996
r1997 = as.data.frame(fread("data/census/race_tract_1997.csv", data.table = FALSE))
r1997 = na.omit(merge(r1997, t1997, by.x = "id", by.y = "block"))
r1997$year = 1997
r1997$lconcentration = log(r1997$concentration)
r1998 = as.data.frame(fread("data/census/race_tract_1998.csv", data.table = FALSE))
r1998 = na.omit(merge(r1998, t1998, by.x = "id", by.y = "block"))
r1998$lconcentration = log(r1998$concentration)
r1998$year = 1998
r1999 = as.data.frame(fread("data/census/race_tract_1999.csv", data.table = FALSE))
r1999 = na.omit(merge(r1999, t1999, by.x = "id", by.y = "block"))
r1999$lconcentration = log(r1999$concentration)
r1999$year = 1999
r2000 = as.data.frame(fread("data/census/race_tract_2000.csv", data.table = FALSE))
r2000 = r2000[, c(3, 12:ncol(r2000))]
names(r2000) = c("id", "pop", "white", "black", "native", "asian", "hawaii", "other", "two")
r2000 = merge(r2000, t2000, by.x = "id", by.y = "block")
r2000$lconcentration = log(r2000$concentration)
r2000$year = 2000
r2001 = as.data.frame(fread("data/census/race_tract_2001.csv", data.table = FALSE))
r2001 = merge(r2001, t2001, by.x = "id", by.y = "block")
r2001$lconcentration = log(r2001$concentration)
r2001$year = 2001
r2002 = as.data.frame(fread("data/census/race_tract_2002.csv", data.table = FALSE))
r2002 = merge(r2002, t2002, by.x = "id", by.y = "block")
r2002$lconcentration = log(r2002$concentration)
r2002$year = 2002
r2003 = as.data.frame(fread("data/census/race_tract_2003.csv", data.table = FALSE))
r2003 = merge(r2003, t2003, by.x = "id", by.y = "block")
r2003$lconcentration = log(r2003$concentration)
r2003$year = 2003
#r2004 = as.data.frame(fread("data/census/race_tract_2004.csv", data.table = FALSE))
r2005 = as.data.frame(fread("data/census/race_tract_2005.csv", data.table = FALSE))
r2005 = merge(r2005, t2005, by.x = "id", by.y = "block")
r2005$lconcentration = log(r2005$concentration)
r2005$year = 2005
r2006 = as.data.frame(fread("data/census/race_tract_2006.csv", data.table = FALSE))
r2006 = merge(r2006, t2006, by.x = "id", by.y = "block")
r2006$lconcentration = log(r2006$concentration)
r2006$year = 2006
r2007 = as.data.frame(fread("data/census/race_tract_2007.csv", data.table = FALSE))
r2007 = merge(r2007, t2007, by.x = "id", by.y = "block")
r2007$lconcentration = log(r2007$concentration)
r2007$year = 2007
r2008 = as.data.frame(fread("data/census/race_tract_2008.csv", data.table = FALSE))
r2008 = merge(r2008, t2008, by.x = "id", by.y = "block")
r2008$lconcentration = log(r2008$concentration)
r2008$year = 2008
r2009 = as.data.frame(fread("data/census/race_tract_2009.csv", data.table = FALSE))
r2009 = merge(r2009, t2009, by.x = "id", by.y = "block")
r2009$lconcentration = log(r2009$concentration)
r2009$year = 2009
r2010 = as.data.frame(fread("data/census/race_tract_2010.csv", data.table = FALSE))
r2010 = r2010[, c(3, 11:ncol(r2010))]
names(r2010) = c("id", "pop", "white", "black", "native", "asian", "hawaii", "other", "two")
r2010 = merge(r2010, t2010, by.x = "id", by.y = "block")
r2010$lconcentration = log(r2010$concentration)
r2010$year = 2010

race_data = rbind.fill(r1990, r1991, r1992, r1993, r1994, r1996, r1997, r1998, r1999, r2000, r2001, r2002, r2003, r2005, r2006, r2007, r2008, r2009, r2010)

#create function to bootstrap an estimate of SE for all races, for a given quantile, in a given year
bootstrap_se = function(year, quant, n = 50000, num = 40) {
  data = race_data[race_data$year == year, ]
  tox = "concentration"
  
  #for each race:
  #bootstrap sample from the toxicity data for that race, 40 times. 
  #calculate the appropriate quantile for each sample
  #calculate the standard deviation for the simulated quantiles
  
  x = replicate(num, sample(data[, tox], n, replace = TRUE, prob = data$white/sum(data$white)), simplify = "array")
  m = apply(x, 2, function(x) quantile(x, quant))
  white_sd = sd(m)
  
  x = replicate(num, sample(data[, tox], n, replace = TRUE, prob = data$black/sum(data$black)), simplify = "array")
  m = apply(x, 2, function(x) quantile(x, quant))
  black_sd = sd(m)
  
  x = replicate(num, sample(data[, tox], n, replace = TRUE, prob = data$native/sum(data$native)), simplify = "array")
  m = apply(x, 2, function(x) quantile(x, quant))
  native_sd = sd(m)
  
  x = replicate(num, sample(data[, tox], n, replace = TRUE, prob = data$asian/sum(data$asian)), simplify = "array")
  m = apply(x, 2, function(x) quantile(x, quant))
  asian_sd = sd(m)
  
  x = replicate(num, sample(data[, tox], n, replace = TRUE, prob = data$other/sum(data$other)), simplify = "array")
  m = apply(x, 2, function(x) quantile(x, quant))
  other_sd = sd(m)
  return(c(year, white_sd, black_sd, native_sd, asian_sd, other_sd))
}

#create function to bootstrap all races, in all years, for a given quantile
get_bootstrap = function(quant) {
  #get the bootstrap estimates of SE for 1990
  errors = bootstrap_se(1990, quant)
  print("done with 1990")
  #for all years after 1990
  for (year in unique(race_data$year)[2:19]) {
    #add another row with the bootstrapped standard deviations for each year
    errors = rbind(errors, bootstrap_se(year, quant))
    print(paste("done with", year))
  }
  
  #export the data.frame with all the relevant estimated standard errors
  errors = as.data.frame(errors)
  names(errors) = c("year", "white_sd", "black_sd", "asian_sd", "native_sd", "other_sd")
  
  write.csv(errors, paste0("data/temp_bootstrap_race_se_", (quant*100), ".csv"))
}

#get the bootstrapped SE's for the quantiles of interest. 
get_bootstrap(0.05)
get_bootstrap(0.50)
get_bootstrap(0.95)
