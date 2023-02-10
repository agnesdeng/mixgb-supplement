# create continuous data for measure time performance
# n_obs: number of observations
# p: number of explanatory variables
# y: numeric response
# Data size: n_obs x (p+1)
create_norm <- function(n_obs = 1000, p = 10) {
  l <- round(p / 2)
  r <- l + 1

  # normal data -------------------------------------------------------------
  # covariance matrix
  cov.m <- matrix(0, ncol = p, nrow = p)
  cov.m[1:l, 1:l] <- 0.5
  cov.m[r:p, r:p] <- 0.5

  diag(cov.m) <- 1

  z <- mvrnorm(n = n_obs, mu = rep(0, p), Sigma = cov.m, empirical = TRUE)
  colnames(z) <- paste0(rep("norm", p), 1:p)
  y <- rowSums(z) + rnorm(n = n_obs, mean = 0, sd = 1)
  full.df <- data.frame(z, y)
  full.df
}
