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
  names(data1) = c("id", "whitel", "whiteh", "blackl", "blackh", "otherl", "otherh", "hisph", "hispl", "pop")
  write.csv(data1, file = paste0("data/census/hisp_pov_tract_", substr(starting, 1, 3), year, ".csv"), row.names = FALSE)
  return(data1)
}

temp1990 = fread("data/census/raw/hpov_1990.csv", data.table = FALSE)
temp1990 = temp1990[, c(7, 12:13, 15:16, 18:19, 21:22, 24:25, 27:28)]
names(temp1990) = c("id", "whiteh", "whitel", "blackh", "blackl", "nativeh", "nativel", "asianh", "asianl", "otherh", "otherl", "hisph", "hispl")
temp1990$otherh = rowSums(temp1990[, c(6, 8, 10)])
temp1990$otherl = rowSums(temp1990[, c(7, 9, 11)])
temp1990 = select(temp1990, id, whiteh, whitel, blackh, blackl, otherh, otherl, hisph, hispl)
temp1990$pop = rowSums(temp1990[, 2:9])
c1990 = crosswalk(cross1990, temp1990, mergex = "id90")

temp2000 = fread("data/census/raw/hpov_2000.csv", data.table = FALSE)
temp2000 = temp2000[, c(3, 13:14, 16:17, 19:20, 22:23, 25:26, 28:29, 31:32, 34:35)]
names(temp2000) = c("id", "whitel", "whiteh", "blackl", "blackh", "nativel", "nativeh", "asianl", "asianh", "hawaiil", "hawaiih", "otherl", "otherh", "twol", "twoh", "hispl", "hisph")
temp2000$otherh = rowSums(temp2000[, c(7, 9, 11, 13, 15)])
temp2000$otherl = rowSums(temp2000[, c(6, 8, 10, 12, 14)])
temp2000 = select(temp2000, id, whiteh, whitel, blackh, blackl, otherh, otherl, hisph, hispl)
temp2000$pop = rowSums(temp2000[, 2:9])
c2000 = crosswalk(cross2000, temp2000)

c2010 = fread("data/census/raw/hpov_2010.csv", data.table = FALSE)
c2010 = c2010[, c(1, 57:58, 60:61, 63:64, 66:67, 69:70, 72:73, 75:76, 78:79)]
names(c2010) = c("id", "whitel", "whiteh", "blackl", "blackh", "nativel", "nativeh", "asianl", "asianh", "hawaiil", "hawaiih", "otherl", "otherh", "twol", "twoh", "hispl", "hisph")
c2010$pop = rowSums(c2010[, 2:17])
c2010$otherh = rowSums(c2010[, c(7, 9, 11, 13, 15)])
c2010$otherl = rowSums(c2010[, c(6, 8, 10, 12, 16)])
c2010 = select(c2010, id, whiteh, whitel, blackh, blackl, otherh, otherl, hisph, hispl, pop)

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

write.csv(c1990, file = "data/census/hisp_pov_tract_1990.csv", row.names = FALSE)
write.csv(c2000, file = "data/census/hisp_pov_tract_2000.csv", row.names = FALSE)
write.csv(c2010, file = "data/census/hisp_pov_tract_2010.csv", row.names = FALSE)