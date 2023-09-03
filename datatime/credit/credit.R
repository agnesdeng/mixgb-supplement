library(microbenchmark)
library(mice)
library(mixgb)


# set working directory to the folder supplement---------------------------------------------------
setwd("C:/Users/agnes/Desktop/phd-thesis/my-projects/mixgb-paper/v5/supplement")

load("datatime/credit/credit.rda")

#this dataset can be downloaded from https://archive.ics.uci.edu/dataset/350/default+of+credit+card+clients

## credit card dataset:  30000 x 24
names(credit.df)[24] <- "DEFAULT"
str(credit.df)


colSums(is.na(credit.df))
# no missing values!

set.seed(2022)
withNA.df <- createNA(credit.df, var.names = c("LIMIT_BAL", "DEFAULT"), p = 0.3)
colSums(is.na(withNA.df))

#

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





credit.time <- microbenchmark(
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
credit.time
credit.sum <- summary(credit.time)
credit.sum


saveRDS(credit.time, file = "C:/Users/agnes/Desktop/phd-thesis/my-projects/mixgb-paper/last-version/supplement/datatime/result/credit0.rds")
saveRDS(credit.sum, file = "C:/Users/agnes/Desktop/phd-thesis/my-projects/mixgb-paper/last-version/supplement/datatime/result/credit.rds")
