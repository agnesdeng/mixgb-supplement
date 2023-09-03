# methods(MIcombine)
# mitools:::MIcombine.default
# micombine.imputationResultList(results)

# The following functions are adapted from functions from R package "mitools".

micombine <- function(results, ...) UseMethod("micombine")


micombine.imputationResultList <- function(results, call = NULL, df.complete = Inf, ...) {
  # VCOV Calculate Variance-Covariance Matrix for a Fitted Model Object
  vars <- suppressWarnings(lapply(results, vcov))
  # coefficient
  thetas <- lapply(results, coef)
  rval <- micombine(thetas, vars, call = sys.call(-1), df.complete = df.complete)
  rval$call <- c(results$call, call)
  rval
}

## adapt the MIcombine function
# results=coef, variances=vcov

micombine <- function(results, variances, call = sys.call(), df.complete = Inf, ...) {
  m <- length(results)
  oldcall <- attr(results, "call")
  if (missing(variances)) {
    # vcov
    variances <- suppressWarnings(lapply(results, vcov))
    # coef
    results <- lapply(results, coef)
  }
  # vcov
  vbar <- variances[[1]]
  # coef estimate
  cbar <- results[[1]]
  for (i in 2:m) {
    cbar <- cbar + results[[i]]
    vbar <- vbar + variances[[i]]
  }
  cbar <- cbar / m
  # vbar: expected within variance
  vbar <- vbar / m
  # evar: expected between variance
  evar <- var(do.call("rbind", results))
  # r: relative increase in variance due to nonresponse
  r <- (1 + 1 / m) * evar / vbar
  # lambda: the proportion of the variation attributable to the missing data
  lambda <- r / (r + 1)
  # df: degree of freedom (rubin old degree of freedom)
  df <- (m - 1) * (1 + 1 / r)^2
  if (is.matrix(df)) {
    df <- diag(df)
  }
  if (is.finite(df.complete)) {
    # adjusted new degree of freedom
    # dfobs <- ((df.complete + 1)/(df.complete + 3)) * df.complete * vbar/(vbar + evar)
    dfobs <- ((df.complete + 1) / (df.complete + 3)) * df.complete * (1 - lambda)
    if (is.matrix(dfobs)) {
      dfobs <- diag(dfobs)
    }
    df <- 1 / (1 / dfobs + 1 / df)
  }
  if (is.matrix(r)) {
    r <- diag(r)
  }
  if (is.matrix(lambda)) {
    lambda <- diag(lambda)
  }
  rval <- list(
    coefficients = cbar, variance = vbar + evar *
      (m + 1) / m, call = c(oldcall, call), nimp = m, df = df,
    missinfo = (r + 2 / (df + 3)) / (r + 1),
    within = diag(vbar),
    between = diag(evar) * (m + 1) / m,
    lambda = lambda
  )
  class(rval) <- "miresult"
  rval
}


vcov.miresult <- function(object, ...) object$variance

miextract <- function(results, expr, fun) {
  pf <- parent.frame()
  if (!is.null(match.call()$expr)) {
    expr <- substitute(expr)
    lapply(results, function(result) eval(expr, result, pf))
  } else {
    lapply(results, fun)
  }
}

print.miresult <- function(x, ...) {
  cat("Multiple imputation results:\n")
  lapply(x$call, function(a) {
    cat("      ")
    print(a)
  })
  out <- data.frame(results = coef(x), se = sqrt(diag(vcov(x))))
  print(out)
}



# matrix version
summary.miresult <- function(object, ..., alpha = 0.05, logeffect = FALSE) {
  # cat("Multiple imputation results:\n")
  # lapply(object$call, function(a) {cat("      ");print(a)})
  # c("comb.coef","total.var","t.se","(t.lower","t.upper)","within","between","missinfo","lambda")
  crit <- qt(alpha / 2, object$df, lower.tail = FALSE)
  Nrow <- length(object$between)
  out <- matrix(NA, nrow = Nrow, ncol = 9)
  # comb.coef
  out[, 1] <- coef(object)
  # total.var
  out[, 2] <- diag(object$variance)

  # b.se:  standard error using adjusted between variance
  # out[, 3] <- sqrt(diag(vcov(object)))
  out[, 3] <- sqrt(object$between)

  # CI uses adjusted between variance
  out[, 4] <- out[, 1] - crit * out[, 3]
  out[, 5] <- out[, 1] + crit * out[, 3]
  #
  out[, 6] <- object$within
  out[, 7] <- object$between
  out[, 8] <- object$missinfo
  out[, 9] <- object$lambda


  if (logeffect) {
    # comb.coef
    out[, 1] <- exp(out[, 1])
    # t.se
    out[, 3] <- out[, 3] * out[, 1]
    # t.lower
    out[, 4] <- exp(out[, 4])
    out[, 5] <- exp(out[, 5])
  }

  # print(out,...)
  out
}
