library(data.table)
library(microbenchmark)
library(mice)
library(mixgb)



# set working directory to the folder supplement---------------------------------------------------
setwd("C:/Users/agnes/Desktop/phd-thesis/my-projects/mixgb-paper/last-version/supplement")


higgs <- fread("datatime/higgs/HIGGS.csv", nrow = 1e6, stringsAsFactors = TRUE)
str(higgs)


higgs$V1 <- as.factor(higgs$V1)
summary(higgs)

set.seed(2022)
withNA.df <- createNA(higgs, var.names = c("V1", "V2"), p = 0.3)
colSums(is.na(higgs))
colSums(is.na(withNA.df))


cpu.params<- list(
  subsample = 1,
  tree_method = "hist"
)

gpu.params <- list(
  nthread = 1L,
  subsample = 1,
  tree_method = "gpu_hist"
)

subcpu.params <- list(
  subsample = 0.7,
  tree_method = "hist"
)

subgpu.params <- list(
  nthread = 1L,
  subsample = 0.7,
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
