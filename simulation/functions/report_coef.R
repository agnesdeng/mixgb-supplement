# obtain the summary table
report_coef <- function(sim.output) {
  true.coefs <- c(
    0, 1, 1, 1, 1, 1, 1,
    -1, -2, 1, 1, -3, -2, 1
  )

  n_estimates <- length(sim.output[[1]]$estimates)

  coef.names <- sim.output[[1]]$estimates

  runs <- length(sim.output[[1]]$sim.time)


  n_methods <- length(names(sim.output))


  summary.list <- list()

  for (i in 1:n_methods) {
    summary.list[[i]] <- as_tibble(sim.output[[i]]$result) %>%
      tibble::add_column(
        estimates = rep(coef.names, runs),
        # coefficients of full data
        hat.coefs = sim.output[[i]]$hat.coefs,
        #
        true.coefs = rep(true.coefs, runs),
        # var of coeffs of full data
        hat.var = sim.output[[i]]$hat.var,
        # coefficients of complete cases
        cp.coefs = sim.output[[i]]$cp.coefs,
        # var of coeffs of complete cases
        cp.var = sim.output[[i]]$cp.var,
      ) %>%
      dplyr::mutate(estimates = factor(estimates, levels = coef.names)) %>%
      dplyr::mutate(
        bias = comb.coefs - hat.coefs,
        # ciwidth = ci.upper - ci.lower,
        coverage = ci.lower < hat.coefs & hat.coefs < ci.upper,
        # coverage = ci.lower < true.coefs & true.coefs < ci.upper,
        .before = comb.coefs
      ) %>%
      group_by(estimates) %>%
      dplyr::summarise(
        mean.bias = mean(bias),
        # mean.ciwidth = mean(ciwidth),
        mean.coverage = mean(coverage),
        mean.total = mean(total),
        mean.within = mean(within),
        mean.between = mean(between),
        mean.missinfo = mean(missinfo),
        mean.lambda = mean(lambda),
        # hat.coefs (same for each run)
        mean.hat.coefs = mean(hat.coefs),
        # (True within) the variance of hat.coefs (same for each run)
        mean.hat.var = mean(hat.var),
        # (True between) variance of the MI estimates over #runs simulation
        var.comb.coefs = var(comb.coefs)
      ) %>%
      dplyr::rename(
        Bias = mean.bias,
        True.Within = mean.hat.var,
        True.Between = var.comb.coefs,
        Total = mean.total,
        Between = mean.between,
        Within = mean.within,
        Coverage = mean.coverage,
        # Width = mean.ciwidth,
        Missinfo = mean.missinfo
      ) %>%
      dplyr::mutate(
        True.Within = 1000 * True.Within,
        True.Between = 1000 * True.Between,
        Total = 1000 * Total,
        Between = 1000 * Between,
        Within = 1000 * Within,
        Coverage = 100 * Coverage,
        Missinfo = 100 * Missinfo,
        True.Total = True.Between + True.Within, .before = True.Between
      ) %>%
      # dplyr::select(Bias,Total,True.Total,Within,True.Within,Between,True.Between,Coverage,Width,Missinfo)
      # dplyr::select(Bias, Total, True.Total, Within, True.Within, Between, True.Between, Coverage, Width)
      dplyr::select(Bias, Total, True.Total, Within, True.Within, Between, True.Between, Coverage)
    # note: between here is after adjustment for small m, ie. between=(1+1/m)*between
    # rubin paper: multiple imputation after 18 years  page 482
  }


  # n_estimates :number of estimates =14
  out <- list()
  for (i in 1:n_estimates) {
    ll <- lapply(summary.list, function(x) x[i, ])
    ll <- do.call(rbind, ll)
    ll <- ll %>% add_column(method = c("mice-default", "mice-cart", "mice-ranger", "mixgb", "mixgb-sub"), .before = "Bias")
    out[[i]] <- ll
  }
  out
}


# without missing info & without CI width
multi_xtable <- function(...) {
  vars <- as.list(match.call(expand.dots = TRUE))[-1]
  df_list <- lapply(vars, eval)
  num_cols <- sapply(df_list, length)
  if (!all(num_cols == num_cols[1])) {
    stop("All data frames must have equal number of columns")
  }
  xtables <- lapply(df_list, function(x) {
    capture.output(xtable::print.xtable(xtable::xtable(x, digits = c(0, 0, 2, 2, 2, 2, 2, 2, 2, 0)), table.placement = "H"))
  })
  if (length(xtables) == 1) {
    return(xtables[[1]])
  }
  header <- xtables[[1]][1:6]
  tail <- xtables[[1]][length(xtables[[1]]) + (-1:0)]
  xtables <- lapply(xtables, function(x) x[7:(length(x) - 2)])
  xtables <- do.call("c", xtables)
  cat(header, xtables, tail, sep = "\n")
}

# without missing info
multi_xtable1 <- function(...) {
  vars <- as.list(match.call(expand.dots = TRUE))[-1]
  df_list <- lapply(vars, eval)
  num_cols <- sapply(df_list, length)
  if (!all(num_cols == num_cols[1])) {
    stop("All data frames must have equal number of columns")
  }
  xtables <- lapply(df_list, function(x) {
    capture.output(xtable::print.xtable(xtable::xtable(x, digits = c(0, 0, 2, 2, 2, 2, 2, 2, 2, 0, 2)), table.placement = "H"))
  })
  if (length(xtables) == 1) {
    return(xtables[[1]])
  }
  header <- xtables[[1]][1:6]
  tail <- xtables[[1]][length(xtables[[1]]) + (-1:0)]
  xtables <- lapply(xtables, function(x) x[7:(length(x) - 2)])
  xtables <- do.call("c", xtables)
  cat(header, xtables, tail, sep = "\n")
}

# with missing info
multi_xtable0 <- function(...) {
  vars <- as.list(match.call(expand.dots = TRUE))[-1]
  df_list <- lapply(vars, eval)
  num_cols <- sapply(df_list, length)
  if (!all(num_cols == num_cols[1])) {
    stop("All data frames must have equal number of columns")
  }
  xtables <- lapply(df_list, function(x) {
    capture.output(xtable::print.xtable(xtable::xtable(x, digits = c(0, 0, 2, 2, 2, 2, 2, 2, 2, 0, 2, 0)), table.placement = "H"))
  })
  if (length(xtables) == 1) {
    return(xtables[[1]])
  }
  header <- xtables[[1]][1:6]
  tail <- xtables[[1]][length(xtables[[1]]) + (-1:0)]
  xtables <- lapply(xtables, function(x) x[7:(length(x) - 2)])
  xtables <- do.call("c", xtables)
  cat(header, xtables, tail, sep = "\n")
}
