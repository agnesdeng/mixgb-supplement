

# set working directory to the folder simulation---------------------------------------------------
setwd("C:/Users/agnes/Desktop/supplement/simulation")

# server
# setwd("/data/users/yden863/sim1000")



source("sim/common.R")
library(mice)
source("sim/sim_rf.R")



full.df <- readRDS("data/full.rds")

sim_rf(
  full.df = full.df, nthread = 8L, runs = 1000, m = 5, maxit = 5, seedset = SEED,
  model.form = mix.form, p = n_estimates,
  folder.dir = "result/rf"
)
