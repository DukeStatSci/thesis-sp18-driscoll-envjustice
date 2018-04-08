#calculate 
data_county = read.csv("../index/data/toxic/toxic_2000_2010_county.csv")
data_tract = read.csv("../index/data/toxic/tract/toxic_2000_2010_tract.csv")
race_tract = read.csv("../index/data/census/race/race_tract_2000.csv")
#1000 on readin
names(data_county) = c("county", "tox")
names(data_tract) = c("tract", "tox", "area")

data_tract = merge(data_tract, race_tract, by.x = "tract", by.y = "id") #300 ms
data_county$diff = NA

for (i in 1:nrow(data_county)){
  county = data_county$county[i]
  tract = data_tract[startsWith(as.character(data_tract$tract), as.character(county)), ]
  black_mean = weighted.mean(log(tract$tox), tract$black)
  white_mean = weighted.mean(log(tract$tox), tract$white)
  data_county$diff[i] = black_mean - white_mean #aka how much higher the mean is for black ppl
}