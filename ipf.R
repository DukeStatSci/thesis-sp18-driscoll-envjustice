dpuma = read_csv("index/data/ipf/puma_data.csv")
puma-tract = read_csv("index/data/ipf/puma_tract.csv")

dpuma = as.data.frame(select(dpuma, puma, race, hispan, ftotinc))
dpuma$dem = NA
for (i in 1:nrow(dpuma)) {
  if (dpuma$hispan[i] != "not hispanic") {
    dpuma$dem[i] = "h"
  } else if (dpuma$race[i] == "white") {
    dpuma$dem[i] = "w"
  } else if (dpuma$race[i] == "black/african american/negro") {
    dpuma$dem[i] = "b"
  } else if (dpuma$race[i] == "chinese" | dpuma$race[i] == "japanese" | dpuma$race[i] == "other asian or pacific islander" | 
             dpuma$race[i] == "three or more major races" | dpuma$race[i] == "two or more races"| dpuma$race[i] == "other race, nec" | 
             dpuma$race[i] == "american indian or alaska native" | dpuma$race[i] == "two major races") {
    dpuma$dem[i] = "o"
  } 
}
dpuma = select(dpuma, puma, ftotinc, dem)