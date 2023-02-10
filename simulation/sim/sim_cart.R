# simulation functions for mice (cart)
sim_cart <- function(full.df, runs = 5, m = 5, maxit = 1, seedset = NULL, model.form, p, folder.dir) {
  if (!dir.exists(file.path(folder.dir))) {
    dir.create(file.path(folder.dir), recursive = TRUE)
  }
  sim.time <- rep(NA, length = runs)


  result.list <- matrix(NA, nrow = p * runs, ncol = 9)

  m.coefs <- matrix(NA, nrow = p * runs, ncol = m)
  imp.set <- paste("m", 1:m, sep = "")
  colnames(m.coefs) <- imp.set

  Measures <- c("comb.coefs", "total", "t.se", "ci.lower", "ci.upper", "within", "between", "missinfo", "lambda")
  colnames(result.list) <- Measures

  hat.coefs <- rep(NA, p * runs)
  hat.var <- rep(NA, p * runs)

  estimates <- character(p)

  cp.coefs <- rep(NA, p * runs)
  cp.var <- rep(NA, p * runs)

  for (r in 1:runs) {
    from <- p * (r - 1) + 1
    to <- p * r

    if (is.null(seedset)) {
      set.seed()
    } else {
      s <- seedset[r]
      set.seed(s)
    }

    ##
    fit <- lm(formula = model.form, data = full.df)
    hat.coefs[from:to] <- coef(fit)
    hat.var[from:to] <- diag(vcov(fit))
    ##
    withNA.df <- mar_mix(full.df)

    cp.fit <- lm(formula = model.form, data = withNA.df)
    cp.coefs[from:to] <- coef(cp.fit)
    cp.var[from:to] <- diag(vcov(cp.fit))

    if (r == 1) {
      estimates <- names(coef(fit))
    }


    # mice.cart
    tic()
    cart.imp <- mice(data = withNA.df, printFlag = FALSE, method = "cart", m = m, maxit = maxit)
    cart.data <- complete(cart.imp, action = "all")
    sim.t <- toc(quiet = T)
    sim.time[r] <- sim.t$toc - sim.t$tic

    cart.fit <- lapply(cart.data, function(dataset) lm(formula = model.form, data = dataset))
    m.coefs[from:to, ] <- do.call(cbind, lapply(cart.fit, coef))
    result.list[from:to, ] <- summary(micombine(cart.fit))


    if (r %% 100 == 0) {

      # save results every n runs
      sim.output <- list("m.coefs" = m.coefs, "result" = result.list, "sim.time" = sim.time, "hat.coefs" = hat.coefs, "hat.var" = hat.var, "estimates" = estimates, "cp.coefs" = cp.coefs, "cp.var" = cp.var)
      filename <- paste(folder.dir, paste("/runs", r, sep = ""), sep = "")
      filename <- paste(filename, ".rds", sep = "")
      saveRDS(sim.output, file = filename)
    }
  }
}
