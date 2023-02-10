#plot the relative ratio
plot_var <- function(sim.output, show.vertical = T) {
  n_estimates <- length(sim.output[[1]]$estimates)

  runs <- length(sim.output[[1]]$sim.time)

  n_methods <- length(names(sim.output))

  coef.names <- sim.output[[1]]$estimates

  summary.list <- list()

  for (i in 1:n_methods) {
    summary.list[[i]] <- as_tibble(sim.output[[i]]$result) %>%
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
      dplyr::select(Bias, Total, True.Total, Within, True.Within, Between, True.Between, Coverage, Width, Missinfo)

    # note: between here is after adjustment for small m, ie. between=(1+1/m)*between
    # rubin paper: multiple imputation after 18 years  page 482
  }

  # summary.list
  all.df <- do.call(rbind.data.frame, summary.list)
  All.tb <- all.df %>%
    tibble::add_column(
      Estimates = factor(rep(sim.output[[1]]$estimates, n_methods), levels = sim.output[[1]]$estimates),
      Methods = factor(rep(c("mice-default", "mice-cart", "mice-ranger", "mixgb", "mixgb-sub"), each = n_estimates),
                       levels = c("mice-default", "mice-cart", "mice-ranger", "mixgb", "mixgb-sub")
      )
    ) %>%
    dplyr::mutate(
      reratio.within = Within / True.Within - 1,
      reratio.between = Between / True.Between -1
    ) %>%
    tidyr::pivot_longer(cols = c(reratio.within, reratio.between), names_to = "reratio.type", values_to = "value") %>%
    dplyr::select(Estimates, Methods, reratio.type, value) %>%
    dplyr::mutate(reratio.type = factor(reratio.type,
                                        levels = c("reratio.within", "reratio.between"),
                                        labels = c(
                                          ' ~Var[W]/Var[W]^{target}~  -1 ',
                                          ' ~Var[B]/Var[B]^{target}~  -1 '
                                        )
    ), )
  # All.df<-as.data.frame(All.tb)

  # '"[" ~Var[W]/Var[W]^{target}~ "]" -1 '
  # '"[" ~Var[B]/Var[B]^{target}~ "]" -1 '

  blues <- c("#00ddff", "#0160c9", "#020da5")
  yellows <- c("#fff701", "#c9ab01", "#a57901")
  greens <- c("#88ff00", "#36be32", "#009353")
  pinks <- c("#ff9fe5", "#db67ce", "#c341bf")
  reds <- c("#ff97a5", "#db505f", "#c32030")

  colors <- c(
    blues[2],
    yellows[2],
    greens[2],
    pinks[2],
    reds[2]
  )

  if (show.vertical) {
    ggplot(data = All.tb, aes(x = Methods, y = value, fill = Methods), labeller = label_parsed) +
      geom_bar(stat = "identity", position = position_dodge()) +
      ylab("Relative ratio") +
      scale_x_discrete() +
      scale_fill_manual(values = colors) +
      facet_grid(Estimates ~ reratio.type, labeller = label_parsed, switch = "y") +
      theme(
        axis.title.x = element_text(size = 18, margin = margin(t = 10, r = 0, b = 0, l = 0), ),
        axis.title.y = element_text(size = 18, margin = margin(0, r = 10, 0, l = 10)),
        axis.text.x = element_text(size = 16, angle = 45, hjust = 1, face = "bold"),
        strip.text.x = element_text(size = 14, face = "bold"),
        strip.text.y = element_text(size = 14, face = "bold"),
        panel.spacing.x = unit(0.05, "in"),
        panel.spacing.y = unit(0.15, "in"),
        legend.position = "bottom",
        legend.box = "horizontal",
        legend.spacing.x = unit(0.2, "in"),
        legend.title = element_text(face = "bold", size = 18),
        legend.text = element_text(face = "bold", size = 14),
        legend.key.width = unit(1.5, "in"),
        legend.key.height = unit(0.3, "in"),
        legend.text.align = 0
      )
  } else {
    ggplot(data = All.tb, aes(x = Methods, y = value, fill = Methods), labeller = label_parsed) +
      geom_bar(stat = "identity", position = position_dodge()) +
      ylab("Relative ratio") +
      scale_x_discrete() +
      scale_fill_manual(values = colors) +
      # scale_fill_discrete(labels = m)+
      # scale_fill_brewer(palette="Set2")+
      # facet_wrap(~quantity.name,scales="free",nrow=2,labeller=label_parsed)+
      facet_grid(reratio.type ~ Estimates, labeller = label_parsed, switch = "y") +
      guides(color = "none", fill = "none") +
      theme(
        axis.title.x = element_text(size = 22, margin = margin(t = 10, r = 0, b = 0, l = 0), ),
        axis.title.y = element_text(size = 22, margin = margin(0, r = 5, 0, l = 0)),
        # axis.text.x = element_text(size = 16, angle = 45, hjust = 1),
        axis.text.x = element_text(size = 17, angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 14),
        strip.text.x = element_text(size = 11, face = "bold"),
        strip.text.y = element_text(size = 12),
        panel.spacing.x = unit(0.05, "in"),
        panel.spacing.y = unit(0.15, "in"),
        legend.position = "bottom",
        legend.box = "horizontal",
        legend.spacing.x = unit(0.17, "in"),
        legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(face = "bold", size = 10),
        legend.key.width = unit(0.2, "in"),
        legend.key.height = unit(0.3, "in"),
        legend.text.align = 0
      )
  }
}




#plot the difference
plot_var0 <- function(sim.output, show.vertical = T) {
  n_estimates <- length(sim.output[[1]]$estimates)

  runs <- length(sim.output[[1]]$sim.time)

  n_methods <- length(names(sim.output))

  coef.names <- sim.output[[1]]$estimates

  summary.list <- list()

  for (i in 1:n_methods) {
    summary.list[[i]] <- as_tibble(sim.output[[i]]$result) %>%
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
      dplyr::select(Bias, Total, True.Total, Within, True.Within, Between, True.Between, Coverage, Width, Missinfo)

    # note: between here is after adjustment for small m, ie. between=(1+1/m)*between
    # rubin paper: multiple imputation after 18 years  page 482
  }

  # summary.list
  all.df <- do.call(rbind.data.frame, summary.list)
  All.tb <- all.df %>%
    tibble::add_column(
      Estimates = factor(rep(sim.output[[1]]$estimates, n_methods), levels = sim.output[[1]]$estimates),
      Methods = factor(rep(c("mice-default", "mice-cart", "mice-ranger", "mixgb", "mixgb-sub"), each = n_estimates),
        levels = c("mice-default", "mice-cart", "mice-ranger", "mixgb", "mixgb-sub")
      )
    ) %>%
    dplyr::mutate(
      diff.within = Within - True.Within,
      diff.between = Between - True.Between
    ) %>%
    tidyr::pivot_longer(cols = c(diff.within, diff.between), names_to = "diff.type", values_to = "value") %>%
    dplyr::select(Estimates, Methods, diff.type, value) %>%
    dplyr::mutate(diff.type = factor(diff.type,
      levels = c("diff.within", "diff.between"),
      labels = c(
        '"[" ~Var[within]-Var[within]({target})~ "]" %*%1000',
        '"[" ~Var[between]-Var[between]({target})~ "]" %*%1000'
      )
    ), )
  # All.df<-as.data.frame(All.tb)


  blues <- c("#00ddff", "#0160c9", "#020da5")
  yellows <- c("#fff701", "#c9ab01", "#a57901")
  greens <- c("#88ff00", "#36be32", "#009353")
  pinks <- c("#ff9fe5", "#db67ce", "#c341bf")
  reds <- c("#ff97a5", "#db505f", "#c32030")

  colors <- c(
    blues[2],
    yellows[2],
    greens[2],
    pinks[2],
    reds[2]
  )

  if (show.vertical) {
    ggplot(data = All.tb, aes(x = Methods, y = value, fill = Methods), labeller = label_parsed) +
      geom_bar(stat = "identity", position = position_dodge()) +
      ylab("Difference") +
      scale_x_discrete() +
      scale_fill_manual(values = colors) +
      facet_grid(Estimates ~ diff.type, labeller = label_parsed, switch = "y") +
      theme(
        axis.title.x = element_text(size = 18, margin = margin(t = 10, r = 0, b = 0, l = 0), ),
        axis.title.y = element_text(size = 18, margin = margin(0, r = 10, 0, l = 10)),
        axis.text.x = element_text(size = 16, angle = 45, hjust = 1, face = "bold"),
        strip.text.x = element_text(size = 14, face = "bold"),
        strip.text.y = element_text(size = 14, face = "bold"),
        panel.spacing.x = unit(0.05, "in"),
        panel.spacing.y = unit(0.15, "in"),
        legend.position = "bottom",
        legend.box = "horizontal",
        legend.spacing.x = unit(0.2, "in"),
        legend.title = element_text(face = "bold", size = 18),
        legend.text = element_text(face = "bold", size = 14),
        legend.key.width = unit(1.5, "in"),
        legend.key.height = unit(0.3, "in"),
        legend.text.align = 0
      )
  } else {
    ggplot(data = All.tb, aes(x = Methods, y = value, fill = Methods), labeller = label_parsed) +
      geom_bar(stat = "identity", position = position_dodge()) +
      ylab("Difference") +
      scale_x_discrete() +
      scale_fill_manual(values = colors) +
      # scale_fill_discrete(labels = m)+
      # scale_fill_brewer(palette="Set2")+
      # facet_wrap(~quantity.name,scales="free",nrow=2,labeller=label_parsed)+
      facet_grid(diff.type ~ Estimates, labeller = label_parsed, switch = "y") +
      guides(color = "none", fill = "none") +
      theme(
        axis.title.x = element_text(size = 22, margin = margin(t = 10, r = 0, b = 0, l = 0), ),
        axis.title.y = element_text(size = 22, margin = margin(0, r = 5, 0, l = 0)),
        # axis.text.x = element_text(size = 16, angle = 45, hjust = 1),
        axis.text.x = element_text(size = 17, angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 14),
        strip.text.x = element_text(size = 11, face = "bold"),
        strip.text.y = element_text(size = 12),
        panel.spacing.x = unit(0.05, "in"),
        panel.spacing.y = unit(0.15, "in"),
        legend.position = "bottom",
        legend.box = "horizontal",
        legend.spacing.x = unit(0.17, "in"),
        legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(face = "bold", size = 10),
        legend.key.width = unit(0.2, "in"),
        legend.key.height = unit(0.3, "in"),
        legend.text.align = 0
      )
  }
}
