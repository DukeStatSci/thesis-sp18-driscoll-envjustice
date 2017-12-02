# Injustice in Toxicity Burdens

Undergraduate thesis of Anne Driscoll, to be submitted in Spring 2018. Using publically available Census and EPA data, seeks to examine the toxicity distribution experienced by minority groups in the US. 

## Contents

* bootstrap_se.R: uses bootstrapping to calculate the standard error of simulated toxicity quantiles.
* bootstrap_se_race.R: uses bootstrapping to calculate the standard error of the observed toxicity quantiles for each race.
* census_crosswalk_extrapolate.R: uses linear interpolation to estimate populations between the 1990 and 2000 census, as well as the 2000 and 2010 census. 
* census_crosswalk_extrapolate_race.R: uses linear interpolation to estimate populations between the 1990 and 2000 census, as well as the 2000 and 2010 census for additional census poverty data. 
* crosswalk_munge.R: creates usable crosswalks between 1990 and 2000 census geography to 2010 geography from the raw NHGIS crosswalks. Aggregates to tract level. Raw crosswalks not included due to github size constraints, so will not run out of box. 
* simulation_data.R: creates the simulated distributions. (Explanation of simulation can be found in Chapter 3 of thesis)

### /index
* *.Rmd: Chapters of thesis

#### /index/data
* bootstrap_race_se_**.csv: output of bootstrap_se_race.R
* bootstrap_simulation_se_**.csv: output of bootstrap_se.R
* simulation**.csv: output of simulation_data.R

#### /index/data/race
* Contains all census race files at tract level

#### /index/data/race_pov
* Contains all census race by poverty files at tract level

#### /index/data/raw
* Contains all census raw files needed to create the usable files


TODO:
- [X] Get hispanic data
- [X] Focus the trendlines, consolidate other
- [X] Visualization of simulation more intuitively, dotted line?
- [ ] If we limit by other factors (income?) do we see equal protection?
- [ ] Can we create a measure for dispproportionate burden for a given area? 
- [ ] Can we include other vulnerable populations? (single mother status, age, health, etc.)
- [ ] Try IPF, given that PUMA distribution assumption is reasonable.
- [ ] Build a longitudinal model with the hisp_data.