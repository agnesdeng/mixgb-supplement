library(data.table)
library(microbenchmark)
library(mice)
library(mixgb)


# set working directory to the folder supplement---------------------------------------------------
setwd("C:/Users/agnes/Desktop/supplement")

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


gpu.params <- list(
  nthread = 1L,
  tree_method = "gpu_hist"
)


# m=5, maxit=1 for all methods
allstate.time <- microbenchmark(
  "mice.default" = {
    mice(data = withNA.df, maxit = 1)
  },
  "mice.cart" = {
    mice(withNA.df, method = "cart", maxit = 1)
  },
  "mice.rf" = {
    mice(withNA.df, method = "rf", rfPackage = "ranger", maxit = 1)
  },
  "mixgb.cpu" = {
    mixgb(data = withNA.df, bootstrap = FALSE)
  },
  "mixgb.gpu" = {
    mixgb(data = withNA.df, bootstrap = FALSE, xgb.params = gpu.params)
  },
  times = 1,
  unit = "s"
)

allstate.time
allstate.sum <- summary(allstate.time)


saveRDS(allstate.time, file = "C:/Users/agnes/Desktop/supplement/datatime/result/allstate0.rds")
saveRDS(allstate.sum, file = "C:/Users/agnes/Desktop/supplement/datatime/result/allstate.rds")
