library(MASS)
library(dplyr)
library(mice)
library(mixgb)
library(microbenchmark)

# set working directory to the folder simtime---------------------------------------------------
setwd("C:/Users/agnes/Desktop/phd-thesis/my-projects/mixgb-paper/v5/supplement/simtime")

source("data/create_norm.R")
source("data/create_MCAR.R")

set.seed(2022)
full.dfA <- create_norm(n_obs = 1e3, p = 10)
full.dfB <- create_norm(n_obs = 1e3, p = 30)
full.dfC <- create_norm(n_obs = 1e3, p = 50)


withNA.dfA <- create_MCAR(full.dfA, var.names = c("norm1", "norm2", "norm3"), p = c(0.5))
withNA.dfB <- create_MCAR(full.dfB, var.names = c("norm1", "norm2", "norm3"), p = c(0.5))
withNA.dfC <- create_MCAR(full.dfC, var.names = c("norm1", "norm2", "norm3"), p = c(0.5))


m <- 5
maxit <- 1
rep.times <- 10



#Hyperparameters settings
cpu.params<- list(
  max_depth = 3, gamma = 0, eta = 0.3, min_child_weight = 1, subsample = 1, colsample_bytree = 1, colsample_bylevel = 1, colsample_bynode = 1,
  tree_method = "hist"
)


gpu.params <- list(
  nthread = 1L, max_depth = 3, gamma = 0, eta = 0.3, min_child_weight = 1, subsample = 1, colsample_bytree = 1, colsample_bylevel = 1, colsample_bynode = 1,
  tree_method = "gpu_hist"
)


subcpu.params<- list(
  max_depth = 3, gamma = 0, eta = 0.3, min_child_weight = 1, subsample = 0.7, colsample_bytree = 1, colsample_bylevel = 1, colsample_bynode = 1,
  tree_method = "hist"
)


subgpu.params <- list(
  nthread = 1L, max_depth = 3, gamma = 0, eta = 0.3, min_child_weight = 1, subsample = 0.7, colsample_bytree = 1, colsample_bylevel = 1, colsample_bynode = 1,
  tree_method = "gpu_hist"
)




withNA.df <- withNA.dfA

A <- microbenchmark(
  "mice.default" = {
    mice(withNA.df, print = T, m = m, maxit = maxit)
  },
  "mice.rf" = {
    mice(withNA.df, method = "rf", rfPackage = "ranger", print = T, m = m, maxit = maxit)
  },
  "mixgb.cpu" = {
    mixgb(data = withNA.df, xgb.params = cpu.params, m = m, maxit = maxit)
  },
  "mixgb.gpu" = {
    mixgb(data = withNA.df, xgb.params = gpu.params, m = m, maxit = maxit)
  },
  "mixgbsub.cpu" = {
    mixgb(data = withNA.df, xgb.params = subcpu.params, m = m, maxit = maxit)
  },
  "mixgbsub.gpu" = {
    mixgb(data = withNA.df, xgb.params = subgpu.params, m = m, maxit = maxit)
  },
  times = rep.times,
  unit = "s"
)

n1e3p10 <- summary(A)
n1e3p10
saveRDS(n1e3p10, file = "result/continuous/n1e3p10.rds")

withNA.df <- withNA.dfB
B <- microbenchmark(
  "mice.default" = {
    mice(withNA.df, print = T, m = m, maxit = maxit)
  },
  "mice.rf" = {
    mice(withNA.df, method = "rf", rfPackage = "ranger", print = T, m = m, maxit = maxit)
  },
  "mixgb.cpu" = {
    mixgb(data = withNA.df, xgb.params = cpu.params, m = m, maxit = maxit)
  },
  "mixgb.gpu" = {
    mixgb(data = withNA.df, xgb.params = gpu.params, m = m, maxit = maxit)
  },
  "mixgbsub.cpu" = {
    mixgb(data = withNA.df, xgb.params = subcpu.params, m = m, maxit = maxit)
  },
  "mixgbsub.gpu" = {
    mixgb(data = withNA.df, xgb.params = subgpu.params, m = m, maxit = maxit)
  },
  times = rep.times,
  unit = "s"
)

n1e3p30 <- summary(B)
n1e3p30
saveRDS(n1e3p30, file = "result/continuous/n1e3p30.rds")

withNA.df <- withNA.dfC
C <- microbenchmark(
  "mice.default" = {
    mice(withNA.df, print = T, m = m, maxit = maxit)
  },
  "mice.rf" = {
    mice(withNA.df, method = "rf", rfPackage = "ranger", print = T, m = m, maxit = maxit)
  },
  "mixgb.cpu" = {
    mixgb(data = withNA.df,  xgb.params = cpu.params, m = m, maxit = maxit)
  },
  "mixgb.gpu" = {
    mixgb(data = withNA.df,  xgb.params = gpu.params, m = m, maxit = maxit)
  },
  "mixgbsub.cpu" = {
    mixgb(data = withNA.df,  xgb.params = subcpu.params, m = m, maxit = maxit)
  },
  "mixgbsub.gpu" = {
    mixgb(data = withNA.df,  xgb.params = subgpu.params, m = m, maxit = maxit)
  },
  times = rep.times,
  unit = "s"
)

n1e3p50 <- summary(C)
n1e3p50
saveRDS(n1e3p50, file = "result/continuous/n1e3p50.rds")
