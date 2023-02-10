mar_mix <- function(data) {
  p <- c(0.6, 0.1, 0.6)

  MAR.data <- marNA(data = data, missing.var = "norm1", depend.2vars = c("y.num", "norm4"), cutoff.probs = c(0, 1 / 3, 2 / 3, 1), missing.probs = p)
  MAR.data <- marNA(data = MAR.data, missing.var = "norm2", depend.2vars = c("y.num", "norm4"), cutoff.probs = c(0, 1 / 3, 2 / 3, 1), missing.probs = p)
  MAR.data <- marNA(data = MAR.data, missing.var = "norm3", depend.2vars = c("y.num", "norm4"), cutoff.probs = c(0, 1 / 3, 2 / 3, 1), missing.probs = p)

  MAR.data <- marNA(data = MAR.data, missing.var = "norm5", depend.2vars = c("y.num", "norm6"), cutoff.probs = c(0, 1 / 3, 2 / 3, 1), missing.probs = p)
  MAR.data <- marNA(data = MAR.data, missing.var = "bin1", depend.2vars = c("y.num", "norm6"), cutoff.probs = c(0, 1 / 3, 2 / 3, 1), missing.probs = p)

  MAR.data <- marNA(data = MAR.data, missing.var = "norm7", depend.2vars = c("y.num", "norm8"), cutoff.probs = c(0, 1 / 3, 2 / 3, 1), missing.probs = p)
  MAR.data <- marNA(data = MAR.data, missing.var = "ord1", depend.2vars = c("y.num", "norm8"), cutoff.probs = c(0, 1 / 3, 2 / 3, 1), missing.probs = p)

  MAR.data
}


# create missing data under MAR
marNA <- function(data, missing.var, depend.2vars, cutoff.probs = c(0, 1 / 3, 2 / 3, 1), missing.probs = c(0.6, 0.1, 0.6)) {
  depend.value <- data[[depend.2vars[1]]] + data[[depend.2vars[2]]]

  cutoff.values <- quantile(depend.value, probs = cutoff.probs, na.rm = T)
  depend.groups <- findInterval(depend.value, vec = cutoff.values, all.inside = TRUE)
  p <- missing.probs[depend.groups]
  miss <- rbinom(n = nrow(data), size = 1, p)
  data[[missing.var]][miss == 1] <- NA
  data
}
