library(readr)
library(data.table)
library(stringr)
library(dplyr)
devtools::install_github("amd112/rseiAnalysis")
library("rseiAnalysis")

#inflation from 1989 to 2014, inclusive. (w/ 2014 as base)
inflate = c(2.02, 1.93, 1.84, 1.74, 1.69, 1.64, 1.60, 1.56, 1.51, 1.47, 1.45, 1.42, 
            1.39, 1.34, 1.32, 1.29, 1.26, 1.23, 1.18, 1.16, 1.11, 1.11, 1.08, 1.06, 
            1.03, 1.02, 1)

#crosswalks from 1990 and 2000 census geographies to 2010 geography
cross2000 = fread("data/census/raw/crosswalk_00_tract.csv", data.table = FALSE)
cross1990 = fread("data/census/raw/crosswalk_90_tract.csv", data.table = FALSE)
cross1990$id10 = str_pad(cross1990$id10, 11, pad = "0", side = "left")
cross1990$id90 = str_pad(cross1990$id90, 11, pad = "0", side = "left")

crosswalk = function(crosswalk, data, mergex = "id00", mergey = "id") {
  #merge the data and the crosswalk
  temp = merge(crosswalk, data, by.x = mergex, by.y = mergey)
  #multiply all the values by the weight of the crosswalk
  temp[, c(4:ncol(temp))] = temp[, c(4:ncol(temp))] * temp$weight
  #take all the crosswalked values
  temp = temp[, c(2, 4:ncol(temp))]
  #group together all the 2010 groups, and add up all the values
  t = temp %>%
    group_by(id10) %>% 
    summarise_all(sum)
  t = as.data.frame(t)
  #give the columns their original names and output
  names(t) = c("id", names(t)[2:ncol(t)])
  return(t)
}

extrapolate = function(data, year, n = 8, starting = "2000") {
  #get data from the start year
  data1 = data[, 2:(n+1)]
  #get data from the end year
  data2 = data[, (n+2):((2*n)+1)]
  #calculate year/10ths the change 
  data_change = year * ((data2 - data1) / 10)
  #add the difference
  data1 = data1 + data_change
  #add the tract number
  data1 = as.data.frame(cbind(data[,1], data1))
  names = c("id", "pop", "white", "black", "native", "asian", "hawaii", "other", "two")
  if (starting == "1990") {names = c("id", "pop", "white", "black", "native", "asian", "other")}
  names(data1) = names
  write.csv(data1, file = paste0("data/census/race_tract_", substr(starting, 1, 3), year, ".csv"), row.names = FALSE)
  return(data1)
}

temp1990 = fread("data/census/raw/race_tract_1990.csv", data.table = FALSE)
temp1990 = temp1990[, c(7, 11:ncol(temp1990))]
names(temp1990) = c("id", "pop", "white", "black", "native", "asian", "other")
c1990 = crosswalk(cross1990, temp1990, mergex = "id90")

temp2000 = fread("data/census/raw/race_tract_2000.csv", data.table = FALSE)
temp2000 = temp2000[, c(3, 12:ncol(temp2000))]
names(temp2000) = c("id", "pop", "white", "black", "native", "asian", "hawaii", "other", "two")
c2000 = crosswalk(cross2000, temp2000)

c2010 = fread("data/census/raw/race_tract_2010.csv", data.table = FALSE)
c2010 = c2010[, c(3, 11:ncol(c2010))]
names(c2010) = c("id", "pop", "white", "black", "native", "asian", "hawaii", "other", "two")

census_00_10 = merge(c2000, c2010, by = "id", all = TRUE)
c2001 = extrapolate(census_00_10, 1)
c2002 = extrapolate(census_00_10, 2)
c2003 = extrapolate(census_00_10, 3)
c2004 = extrapolate(census_00_10, 4)
c2005 = extrapolate(census_00_10, 5)
c2006 = extrapolate(census_00_10, 6)
c2007 = extrapolate(census_00_10, 7)
c2008 = extrapolate(census_00_10, 8)
c2009 = extrapolate(census_00_10, 9)

census_90_10 = merge(c1990, c2000, by = "id", all = TRUE)
c1991 = extrapolate(census_90_10, 1, 6, starting = "1990")
c1992 = extrapolate(census_90_10, 2, 6, starting = "1990")
c1993 = extrapolate(census_90_10, 3, 6, starting = "1990")
c1994 = extrapolate(census_90_10, 4, 6, starting = "1990")
c1995 = extrapolate(census_90_10, 5, 6, starting = "1990")
c1996 = extrapolate(census_90_10, 6, 6, starting = "1990")
c1997 = extrapolate(census_90_10, 7, 6, starting = "1990")
c1998 = extrapolate(census_90_10, 8, 6, starting = "1990")
c1999 = extrapolate(census_90_10, 9, 6, starting = "1990")