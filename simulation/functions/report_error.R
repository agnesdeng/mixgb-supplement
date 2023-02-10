
# Monte Carlo SEs ---------------------------------------------------------
mce_rrors <- function(sim.output) {
  true.coefs <- c(
    0, 1, 1, 1, 1, 1, 1,
    -1, -2, 1, 1, -3, -2, 1
  )

  n_estimates <- length(sim.output[[1]]$estimates)

  coef.names <- sim.output[[1]]$estimates

  runs <- length(sim.output[[1]]$sim.time)


  n_methods <- length(names(sim.output))


  # colnames(sim.output[[1]]$result)


  mce.list <- list()

  for (i in 1:n_methods) {
    mce.list[[i]] <- as_tibble(sim.output[[i]]$result) %>%
      tibble::add_column(
        estimates = rep(coef.names, runs),
        # coefficients of full data
        hat.coefs = sim.output[[i]]$hat.coefs,
        true.coefs = rep(true.coefs, runs),
        # var of coefs of full data
        hat.var = sim.output[[i]]$hat.var,
      ) %>%
      dplyr::mutate(estimates = factor(estimates, levels = coef.names)) %>%
      dplyr::mutate(
        Bias = comb.coefs - hat.coefs,
        Width = ci.upper - ci.lower,
        Coverage = 100 * (ci.lower < true.coefs & true.coefs < ci.upper),
        # Coverage=100*(ci.lower< hat.coefs & hat.coefs <ci.upper),
        # Missinfo=100*missinfo,
        .before = comb.coefs
      ) %>%
      # dplyr::select(Bias,Coverage,Width,Missinfo,estimates)%>%
      #dplyr::select(Bias, Coverage, Width, estimates) %>%
      dplyr::select(Bias, Coverage, estimates) %>%
      group_by(estimates) %>%
      dplyr::summarise(across(everything(), list(se = std.error)))
  }



  out <- list()
  for (i in 1:n_estimates) {
    ll <- lapply(mce.list, function(x) x[i, ])
    ll <- do.call(rbind, ll)
    out[[i]] <- ll
  }

  S <- sapply(out, function(x) apply(x[, -1], 2, max))
  # dim(S)
  apply(S, 1, max)
}



boot_error <- function(sim.output, boot.runs = 100) {
  n_estimates <- length(sim.output[[1]]$estimates)

  coef.names <- sim.output[[1]]$estimates

  n_methods <- length(names(sim.output))

  d <- n_methods * n_estimates

  M <- matrix(NA, nrow = d * boot.runs, ncol = 6)

  for (r in 1:boot.runs) {
    from <- (r - 1) * d + 1
    to <- r * d
    B <- boot_coef(sim.output)
    M[from:to, ] <- as.matrix(do.call(rbind.data.frame, B))
  }


  M.df <- as.data.frame(M)
  M.df$methods <- factor(rep(rep(c("mice.default", "mice.cart", "mice.rf", "mixgb", "mixgb.boot"), n_estimates), boot.runs), levels = c("mice.default", "mice.cart", "mice.rf", "mixgb", "mixgb.boot"))
  M.df$estimates <- factor(rep(rep(coef.names, each = n_methods), boot.runs), levels = coef.names)
  colnames(M.df) <- c("total", "true.total", "within", "true.within", "between", "true.between", "methods", "estimates")

  # M.df %>% filter(methods=="mice.default",estimates=="norm1")


  boot.sum <- M.df %>%
    group_by(estimates, methods) %>%
    dplyr::summarise(across(everything(), list(se = std.error)))

  apply(boot.sum[, -c(1, 2)], 2, max)
}



boot_coef <- function(sim.output) {
  n_estimates <- length(sim.output[[1]]$estimates)

  # bootstrap sample of simulation IDs
  idx <- sample(1000, size = 1000, replace = TRUE)
  # the location of the index (first estimate)
  from <- (idx - 1) * n_estimates + 1
  # the location of the index (1-14 estimates)
  boot.index <- rep(from, each = n_estimates) + 0:(n_estimates - 1)


  coef.names <- sim.output[[1]]$estimates

  runs <- length(sim.output[[1]]$sim.time)

  n_methods <- length(names(sim.output))


  summary.list <- list()

  for (i in 1:n_methods) {
    summary.list[[i]] <- as_tibble(sim.output[[i]]$result[boot.index, ]) %>%
      tibble::add_column(
        estimates = rep(coef.names, runs),
        # coefficients of full data
        hat.coefs = sim.output[[i]]$hat.coefs,
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
        ciwidth = ci.upper - ci.lower,
        coverage = ci.lower < hat.coefs & hat.coefs < ci.upper,
        .before = comb.coefs
      ) %>%
      group_by(estimates) %>%
      dplyr::summarise(
        mean.bias = mean(bias),
        mean.ciwidth = mean(ciwidth),
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
        Width = mean.ciwidth,
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
      dplyr::select(Total, True.Total, Within, True.Within, Between, True.Between)

    # note: between here is after adjustment for small m, ie. between=(1+1/m)*between
    # rubin paper: multiple imputation after 18 years  page 482
  }

  # n_estimates :number of estimates =14
  out <- list()
  for (i in 1:n_estimates) {
    ll <- lapply(summary.list, function(x) x[i, ])
    ll <- do.call(rbind, ll)
    out[[i]] <- ll
  }
  out
}
