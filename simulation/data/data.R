
#Set working directory to the data folder
setwd("C:/Users/agnes/Desktop/phd-thesis/my-projects/mixgb-paper/last-version/supplement/simulation/data")


source("create_mix.R")

library(MASS)
library(dplyr)

set.seed(2022)

full.df<-create_mix(n_obs = 1e4)
saveRDS(full.df,file="full.rds")

