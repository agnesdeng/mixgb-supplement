library(data.table)
library(microbenchmark)
library(mice)
library(mixgb)



# set working directory to the folder supplement---------------------------------------------------
setwd("C:/Users/agnes/Desktop/phd-thesis/my-projects/mixgb-paper/v5/supplement")

#the full HIGGS.csv dataset can be downloaed from https://archive.ics.uci.edu/dataset/280/higgs
#Here we only take the first 1 million samples
higgs <- fread("datatime/higgs/HIGGS.csv", nrow = 1e6, stringsAsFactors = TRUE)
str(higgs)


higgs$V1 <- as.factor(higgs$V1)
summary(higgs)

set.seed(2022)
withNA.df <- createNA(higgs, var.names = c("V1", "V2"), p = 0.3)
colSums(is.na(higgs))
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



higgs1M.time <- microbenchmark(
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

higgs1M.time
higgs1M.sum <- summary(higgs1M.time)

saveRDS(higgs1M.time, file = "C:/Users/agnes/Desktop/phd-thesis/my-projects/mixgb-paper/last-version/supplement/datatime/result/higgs1M0.rds")
saveRDS(higgs1M.sum, file = "C:/Users/agnes/Desktop/phd-thesis/my-projects/mixgb-paper/last-version/supplement/datatime/result/higgs1M.rds")
