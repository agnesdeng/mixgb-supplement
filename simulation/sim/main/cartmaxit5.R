# set working directory to the folder simulation---------------------------------------------------


# server
setwd("/data/users/yden863/simulation")

source("sim/common.R")
library(mice)
source("sim/sim_cart.R")


full.df <- readRDS("data/full.rds")



sim_cart(
  full.df = full.df, runs = 1000, m = 5, maxit = 5, seedset = SEED,
  model.form = mix.form, p = n_estimates,
  folder.dir = "result/cart"
)
