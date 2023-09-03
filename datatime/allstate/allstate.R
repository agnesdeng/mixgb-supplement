library(data.table)
library(microbenchmark)
library(mice)
library(mixgb)


# set working directory to the folder supplement---------------------------------------------------
setwd("C:/Users/agnes/Desktop/phd-thesis/my-projects/mixgb-paper/v5/supplement")

# allstate dataset (train.csv) can be downloaded from:
# https://www.kaggle.com/competitions/allstate-claims-severity/data

allstate <- fread("datatime/allstate/train.csv", drop = "id", stringsAsFactors = TRUE)
str(allstate)
summary(allstate)
colnames(allstate)

set.seed(2022)
withNA.df <- createNA(allstate, var.names = c("cat1", "loss"), p = 0.3)
colSums(is.na(allstate))
colSums(is.na(withNA.df))


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



allstate.time <- microbenchmark(
  "mice.default" = {
    mice(withNA.df, maxit = 1)
  },
  "mice.cart" = {
    mice(withNA.df, method = "cart", maxit = 1)
  },
  "mice.rf" = {
    mice(withNA.df, method = "rf", rfPackage = "ranger", maxit = 1)
  },
  "mixgb.cpu" = {
    mixgb(data = withNA.df, xgb.params = cpu.params)
  },
  "mixgb.gpu" = {
    mixgb(data = withNA.df, xgb.params = gpu.params)
  },
  "mixgbsub.cpu" = {
    mixgb(data = withNA.df, xgb.params = subcpu.params)
  },
  "mixgbsub.gpu" = {
    mixgb(data = withNA.df, xgb.params = subgpu.params)
  },
  times = 10,
  unit = "s"
)

allstate.time
allstate.sum <- summary(allstate.time)
allstate.sum

saveRDS(allstate.time, file = "C:/Users/agnes/Desktop/phd-thesis/my-projects/mixgb-paper/last-version/supplement/datatime/result/allstate0.rds")
saveRDS(allstate.sum, file = "C:/Users/agnes/Desktop/phd-thesis/my-projects/mixgb-paper/last-version/supplement/datatime/result/allstate.rds")
