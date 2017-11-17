library(readr)
library(dplyr)
library(plyr)
library(data.table)
devtools::install_github("amd112/rseiAnalysis")
library("rseiAnalysis")
#needed for RSEI
set.seed(1294045)

#read in race and toxicity data for 1990 and 2000
t1990 = as.data.frame(fread("data/toxic/tract/toxic_1990_2010_tract.csv", data.table = FALSE))
t2000 = as.data.frame(fread("data/toxic/tract/toxic_2000_2010_tract.csv", data.table = FALSE))
r1990 = as.data.frame(fread("data/census/race_tract_1990.csv", data.table = FALSE))
r1990 = r1990[, c(7, 11:ncol(r1990))]
names(r1990) = c("id", "pop", "white", "black", "native", "asian", "other")
r1990 = merge(r1990, t1990, by.x = "id", by.y = "block")
r1990$lconcentration = log(r1990$concentration)
r1990$year = 1990
r2000 = as.data.frame(fread("data/census/race_tract_2000.csv", data.table = FALSE))
r2000 = r2000[, c(3, 12:ncol(r2000))]
names(r2000) = c("id", "pop", "white", "black", "native", "asian", "hawaii", "other", "two")
r2000 = merge(r2000, t2000, by.x = "id", by.y = "block")
r2000$lconcentration = log(r2000$concentration)
r2000$year = 2000

#create function to bootstrap uncertainty for different levels of n
f = function(y, x, quant) {
  df = data.frame(n = rep(NA, length(y)), sd = rep(NA, length(y)), mean = rep(NA, length(y)))
  for (i in 1:length(y)) {
    #for every n defined
    n = y[i]
    #create a sample of size n 20 times
    x[[i]] = replicate(20, extrapolate(r2000, r1990, "black", n = n), simplify = "array")
    #find the xth quantile for each of those 20 simulations
    m = apply(x[[i]], 2, function(x) quantile(x, quant))
    #find the sd of the xth quantiles
    sd = sd(m)
    df[i, ] = c(y[i], sd, mean(m))
    print(paste("finished calculating for n =", n))
  }
  #write the data
  write.csv(df, paste0("data/bootstrap_simulation_se_", (quant*100), ".csv"), row.names = FALSE)
  return(df)
}

y = c(seq(100, 1000, 100), seq(2000, 10000, 1000), seq(30000, 130000, 20000))
x = list(rep(NA, length(y)))

#create bootstrap estimates of simulation accuracy for each of the chosen quantiles
p05 = f(y, x, 0.05)
p50 = f(y, x, 0.50)
p95 = f(y, x, 0.95)

