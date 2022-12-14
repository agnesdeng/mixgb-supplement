library(data.table)
library(microbenchmark)
library(mice)
library(mixgb)



# set working directory to the folder supplement---------------------------------------------------
setwd("C:/Users/agnes/Desktop/supplement")

higgs <- fread("datatime/higgs/HIGGS.csv", nrow = 1e6, stringsAsFactors = TRUE)
str(higgs)


higgs$V1 <- as.factor(higgs$V1)
summary(higgs)

set.seed(2022)
withNA.df <- createNA(higgs, var.names = c("V1", "V2"), p = 0.3)
colSums(is.na(higgs))
colSums(is.na(withNA.df))


gpu.params <- list(
  nthread = 1L,
  tree_method = "gpu_hist"
)

# m=5, maxit=1 for all methods
higgs1M.time <- microbenchmark(
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

higgs1M.time

saveRDS(higgs1M.time, file = "C:/Users/agnes/Desktop/supplement/datatime/result/higgs1M0.rds")
saveRDS(higgs1M.sum, file = "C:/Users/agnes/Desktop/supplement/datatime/result/higgs1M.rds")
