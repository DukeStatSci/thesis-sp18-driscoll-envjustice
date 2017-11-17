library(readr)
library(data.table)
library(dplyr)
library(stringr)

## Raw census data not included in package due to github size constraints.
## Must get the nhgis_blk_1990_blk2010 files to be able to run.

#import the data
cw90 = as.data.frame(fread("data/census/raw/nhgis_blk1990_blk2010_ge.csv"))
names(cw90) = c("id90", "id10", "weight", "area")
#remove the rows that don't reference a 1990 block
cw90 = filter(cw90, id90 != "")
#take the first 12 chars from 1990 block
cw90$id90 = substr(cw90$id90, 1, 11)
#convert to char, pad with '0', and take first 12 char from 2010 block
cw90$id10 = as.character(cw90$id10)
cw90$id10 = str_pad(cw90$id10, 15, "left", pad = "0")
cw90$id10 = substr(cw90$id10, 1, 11)

#repeat for 2000
cw00 = as.data.frame(fread("data/census/raw/nhgis_blk2000_blk2010_ge.csv"))
names(cw00) = c("id00", "id10", "weight", "area")
cw00 = filter(cw00, id00 != "")
cw00$id00 = str_pad(cw00$id00, 15, "left", pad = "0")
cw00$id00 = substr(cw00$id00, 1, 11)
cw00$id10 = as.character(cw00$id10)
cw00$id10 = str_pad(cw00$id10, 15, "left", pad = "0")
cw00$id10 = substr(cw00$id10, 1, 11)

w90 = cw90 %>%
  group_by(id90) %>%
  summarise(tot_weight = sum(weight))
w90 = as.data.frame(w90)
w90 = merge(cw90, w90, by = "id90") 
cw90 = w90 %>%
  group_by(id90, id10) %>%
  summarize(weight = sum(weight)/min(tot_weight))
write.csv(cw90, file = "data/census/raw/crosswalk_90_tract.csv", row.names = FALSE)

w00 = cw00 %>%
  group_by(id00) %>%
  summarise(tot_weight = sum(weight))
w00 = as.data.frame(w00)
w00 = merge(cw00, w00, by = "id00") 
cw00 = w00 %>%
  group_by(id00, id10) %>%
  summarize(weight = sum(weight)/min(tot_weight))
write.csv(cw00, file = "data/census/raw/crosswalk_00_tract.csv", row.names = FALSE)