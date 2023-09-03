

# set working directory to the folder simulation---------------------------------------------------

# server
setwd("/path_to_directory/simulation")


source("sim/common.R")
library(mixgb)
source("sim/sim_mixgb.R")



full.df <- readRDS("data/full.rds")



sim_mixgb(
  full.df = full.df, bootstrap = FALSE, nthread = 8L, subsample = 1, runs = 1000, m = 5, maxit = 1, seedset = SEED,
  model.form = mix.form, p = n_estimates,
  folder.dir = "result/mixgb"
)
