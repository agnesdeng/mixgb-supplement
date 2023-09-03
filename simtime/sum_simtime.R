# summarise computational time for different combinations of
# n: the number of observations
# p: the number of explanatory variables
# nlevels: the number of levels in each categorical variable



# results: the summary of microbenchmark result (data.frame)
sum_simtime <- function(result) {
  res.string <- deparse(substitute(result))

  # characters between n and p
  n_obs <- stringr::str_extract(str_extract(res.string, "n[[:alnum:]]*p"), "[^npL]+")
  # characters between p and end of string.
  n_p <- str_extract(str_extract(res.string, "p[[:alnum:]]*$"), "[^npL]+")

  result$n_obs <- paste(n_obs, "samples")
  result$n_p <- paste(n_p, "expvars")

  result
}
