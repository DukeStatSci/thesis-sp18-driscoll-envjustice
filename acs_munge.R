library(readr)
library(data.table)
library(dplyr)
library(stringr)

read_in = function(year) {
  acs11 = fread(paste0("index/data/census/raw/acs", year, ".csv"), data.table = FALSE, skip = 1)
  acs_hisp = acs11[, c(1, 56, 58:72)]
  acs_hisp$white = rowSums(acs_hisp[,c(3, 11)])
  acs_hisp$black = rowSums(acs_hisp[,c(4, 12)])
  acs_hisp$native = rowSums(acs_hisp[,c(5, 13)])
  acs_hisp$asian = rowSums(acs_hisp[,c(6, 14)])
  acs_hisp$hawaii = rowSums(acs_hisp[,c(7, 15)])
  acs_hisp$other = rowSums(acs_hisp[,c(8, 16)])
  acs_hisp$two = rowSums(acs_hisp[,c(9, 17)])
  acs_hisp$hispanic = acs_hisp[, 10]
  acs_hisp$id = acs_hisp[, 1]
  acs_hisp$pop = acs_hisp[, 2]
  acs_hisp = select(acs_hisp, id, pop, white, black, native, asian, hawaii, other, two, hispanic)
  write.csv(acs_hisp, paste0("index/data/census/hisp/hisp_tract_", year, ".csv"), row.names = FALSE)
  
  acs_pov = acs11[, c(1, 74:75, 77:78, 80:81, 83:84, 86:87, 89:90, 92:93, 95:96)]
  names(acs_pov) = c("id", "whitel", "whiteh", "blackl", "blackh", "nativel", "nativeh", "asianl", "asianh", 
                     "hawaiil", "hawaiih", "otherl", "otherh", "twol", "twoh", "hispl", 'hisph')
  acs_pov$otherl = rowSums(acs_pov[, c(6, 8, 10, 12, 14)])
  acs_pov$otherh = rowSums(acs_pov[, c(7, 9, 11, 13, 15)])
  acs_pov = select(acs_pov, id, whiteh, whitel, blackh, blackl, otherh, otherl, hisph, hispl)
  acs_pov$pop = rowSums(acs_pov[, -1])
  write.csv(acs_pov, paste0("index/data/census/hisp_pov/hisp_pov_tract_", year, ".csv"), row.names = FALSE)
}

read_in(2011)
read_in(2012)
read_in(2013)
