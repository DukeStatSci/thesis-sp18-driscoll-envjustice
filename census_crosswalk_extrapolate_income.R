library(readr)
library(data.table)
library(stringr)
library(dplyr)
devtools::install_github("amd112/rseiAnalysis")
library("rseiAnalysis")

#crosswalks from 1990 and 2000 census geographies to 2010 geography
cross2000 = fread("data/census/raw/crosswalk_00_tract.csv", data.table = FALSE)
cross1990 = fread("data/census/raw/crosswalk_90_tract.csv", data.table = FALSE)
cross1990$id10 = str_pad(cross1990$id10, 11, pad = "0", side = "left")
cross1990$id90 = str_pad(cross1990$id90, 11, pad = "0", side = "left")

crosswalk = function(crosswalk, data, mergex = "id00", mergey = "id") {
  #merge the data and the crosswalk
  temp = merge(crosswalk, data, by.x = mergex, by.y = mergey)
  #multiply all the values by the weight of the crosswalk
  temp[, c(4:ncol(temp))] = temp[, c(4:ncol(temp))] * as.numeric(temp$weight)
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

extrapolate = function(data, year, n = 9, starting = "2000") {
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
  names(data1) = c("id", "whitel", "whitem", "whiteh", "blackl", "blackm", "blackh", "hisph", "hispm", "hispl")
  write.csv(data1, file = paste0("data/census/income_tract_", substr(starting, 1, 3), year, ".csv"), row.names = FALSE)
  return(data1)
}

temp1990 = fread("data/census/ipums/nhgis0001_ds123_1990_tract.csv", data.table = FALSE)
hisp1990 = fread("data/census/ipums/nhgis0002_ds123_1990_tract.csv", data.table = FALSE)
temp1990 = merge(temp1990, hisp1990)

temp1990$whitel = rowSums(temp1990[, 27:30])
temp1990$whitem = rowSums(temp1990[, 31:33])
temp1990$whiteh = rowSums(temp1990[, 34:35])
temp1990$blackl = rowSums(temp1990[, 36:39])
temp1990$blackm = rowSums(temp1990[, 40:42])
temp1990$blackh = rowSums(temp1990[, 43:44])
temp1990$hispl = rowSums(temp1990[, 72:75])
temp1990$hispm = rowSums(temp1990[, 76:78])
temp1990$hisph = rowSums(temp1990[, 79:80])
temp1990$id = paste0(temp1990$STATEA, temp1990$COUNTYA, str_pad(temp1990$TRACTA, 6, "right", pad = "0"))
c1990 = select(temp1990, id, whitel, whitem, whiteh, blackl, blackm, blackh, hispl, hispm, hisph)
c1990 = crosswalk(cross1990, c1990, mergex = "id90")

temp2000 = fread("data/census/ipums/nhgis0001_ds151_2000_tract.csv", data.table = FALSE)
hisp2000 = fread("data/census/ipums/nhgis0002_ds151_2000_tract.csv", data.table = FALSE)
temp2000 = merge(temp2000, hisp2000)
temp2000$whitel = rowSums(temp2000[, 32:35])
temp2000$whitem = rowSums(temp2000[, 36:42])
temp2000$whiteh = rowSums(temp2000[, 43:47])
temp2000$blackl = rowSums(temp2000[, 48:51])
temp2000$blackm = rowSums(temp2000[, 52:58])
temp2000$blackh = rowSums(temp2000[, 59:63])
temp2000$hispl = rowSums(temp2000[, 144:147])
temp2000$hispm = rowSums(temp2000[, 148:154])
temp2000$hisph = rowSums(temp2000[, 155:159])
temp2000$id = paste0(temp2000$STATEA, temp2000$COUNTYA, str_pad(temp2000$TRACTA, 6, "right", pad = "0"))
c2000 = select(temp2000, id, whitel, whitem, whiteh, blackl, blackm, blackh, hispl, hispm, hisph)
c2000 = crosswalk(cross2000, c2000, mergex = "id00")

w2010 = fread("data/census/ipums/ACS_10_5YR_white.csv", data.table = FALSE, skip = 1)
w2010$id = str_pad(w2010$Id2, 11, "left", pad = "0")
w2010$whitel = rowSums(w2010[, c(6, 8, 10, 12)])
w2010$whitem = rowSums(w2010[, c(14, 16, 18, 20, 22, 24, 26)])
w2010$whiteh = rowSums(w2010[, c(28, 30, 32, 34, 36)])
w2010 = select(w2010, id, whitel, whitem, whiteh)

b2010 = fread("data/census/ipums/ACS_10_5YR_black.csv", data.table = FALSE, skip = 1)
b2010$id = str_pad(b2010$Id2, 11, "left", pad = "0")
b2010$blackl = rowSums(b2010[, c(6, 8, 10, 12)])
b2010$blackm = rowSums(b2010[, c(14, 16, 18, 20, 22, 24, 26)])
b2010$blackh = rowSums(b2010[, c(28, 30, 32, 34, 36)])
b2010 = select(b2010, id, blackl, blackm, blackh)

h2010 = fread("data/census/ipums/ACS_10_5YR_hisp.csv", data.table = FALSE, skip = 1)
h2010$id = str_pad(h2010$Id2, 11, "left", pad = "0")
h2010$hispl = rowSums(h2010[, c(6, 8, 10, 12)])
h2010$hispm = rowSums(h2010[, c(14, 16, 18, 20, 22, 24, 26)])
h2010$hisph = rowSums(h2010[, c(28, 30, 32, 34, 36)])
h2010 = select(h2010, id, hispl, hispm, hisph)

c2010 = merge(w2010, b2010)
c2010 = merge(c2010, h2010)

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
c1991 = extrapolate(census_90_10, 1, starting = "1990")
c1992 = extrapolate(census_90_10, 2, starting = "1990")
c1993 = extrapolate(census_90_10, 3, starting = "1990")
c1994 = extrapolate(census_90_10, 4, starting = "1990")
c1995 = extrapolate(census_90_10, 5, starting = "1990")
c1996 = extrapolate(census_90_10, 6, starting = "1990")
c1997 = extrapolate(census_90_10, 7, starting = "1990")
c1998 = extrapolate(census_90_10, 8, starting = "1990")
c1999 = extrapolate(census_90_10, 9, starting = "1990")

write.csv(c1990, file = "data/census/income_tract_1990.csv", row.names = FALSE)
write.csv(c2000, file = "data/census/income_tract_2000.csv", row.names = FALSE)
write.csv(c2010, file = "data/census/income_tract_2010.csv", row.names = FALSE)