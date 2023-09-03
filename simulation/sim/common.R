library(MASS)
library(bindata)
library(GenOrd)
library(dplyr)
library(tictoc)

source("data/create_mix.R")
source("data/mar_mix.R")
source("sim/utils.R")

# formula
mix.form <- as.formula(y.num ~ norm1 + norm2 + norm3 +
  norm5 +
  norm7 +
  bin1 +
  ord1 +
  I(norm1^2) + norm2:norm3 + norm5:bin1 + norm7:ord1)

# p
n_estimates <- 14

# SEED set for reproducible results
set.seed(2022, kind = "L'Ecuyer-CMRG")
SEED <- round(runif(1000, min = 0, max = 1000000))
