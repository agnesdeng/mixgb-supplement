library(microbenchmark)
library(mice)
library(mixgb)


# set working directory to the folder supplement---------------------------------------------------
setwd("C:/Users/agnes/Desktop/supplement")

load("datatime/credit/credit.rda")

## credit card dataset:  30000 x 24
names(credit.df)[24] <- "DEFAULT"
str(credit.df)


colSums(is.na(credit.df))
# no missing values!

set.seed(2022)
withNA.df <- createNA(credit.df, var.names = c("LIMIT_BAL", "DEFAULT"), p = 0.3)
colSums(is.na(withNA.df))


gpu.params <- list(
  nthread = 1L,
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
    mixgb(data = withNA.df, bootstrap = FALSE)
  },
  "mixgb.gpu" = {
    mixgb(data = withNA.df, bootstrap = FALSE, xgb.params = gpu.params)
  },
  times = 1,
  unit = "s"
)
credit.time
credit.sum <- summary(credit.time)
credit.sum


saveRDS(credit.time, file = "C:/Users/agnes/Desktop/supplement/datatime/result/credit0.rds")
saveRDS(credit.sum, file = "C:/Users/agnes/Desktop/supplement/datatime/result/credit.rds")
