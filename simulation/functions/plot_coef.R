plot_coef <- function(sim.output) {
  n_output <- length(sim.output)
  n_estimates <- length(sim.output[[1]]$estimates)

  Runs <- rep(NA, n_output)
  for (i in 1:n_output) {
    Runs[i] <- sim.output[[i]]$sim.time %>%
      na.omit() %>%
      length()
  }

  hat.runs <- max(Runs)
  maxruns.idx <- which.max(Runs)
  full.runs <- length(sim.output[[1]]$sim.time)


  coefs.tb <- sim.output %>%
    lapply(function(x) x[["result"]][, "comb.coefs"]) %>%
    as_tibble() %>%
    tibble::add_column(
      Estimates = rep(sim.output[[1]]$estimates, times = full.runs),
      hat.coefs = sim.output[[maxruns.idx]]$hat.coefs,
      cp.coefs = sim.output[[maxruns.idx]]$cp.coefs
    ) %>%
    tidyr::pivot_longer(
      !Estimates,
      names_to = "Methods",
      values_to = "coefs"
    ) %>%
    dplyr::mutate(
      Methods = factor(Methods, levels = c("hat.coefs", "cp.coefs", names(sim.output))),
      Estimates = factor(Estimates, levels = sim.output[[1]]$estimates)
    ) %>%
    drop_na()


  true.line <- coefs.tb %>%
    dplyr::filter(Methods == "hat.coefs") %>%
    dplyr::group_by(Estimates) %>%
    dplyr::summarise(mean.hat = mean(coefs))

  labels.names <- c("empirical true", "complete cases", "mice-default", "mice-cart", "mice-ranger", "mixgb", "mixgb-sub")


  # light to dark
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


  # colors<-c("black","gray30",colors)
  colors <- c("black", "gray30", colors)

  ggplot(data = coefs.tb, aes(x = coefs, y = Methods, color = Methods, fill = Methods), labeller = label_parsed) +
    geom_boxplot(aes(fill = Methods), width = 0.2, outlier.color = NA, alpha = 0.5) +
    facet_wrap(vars(Estimates), scales = "free_x", nrow = 2, labeller = label_parsed) +
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors) +
    scale_x_continuous(breaks = breaks_fun) +
    scale_y_discrete(labels = labels.names) +
    guides(color = "none", fill = "none") +
    labs(x = "Coefficient", y = "Method") +
    theme(
      axis.title.x = element_text(size = 26, margin = margin(t = 10, r = 0, b = 0, l = 0), ),
      axis.title.y = element_text(size = 26, margin = margin(0, r = 10, 0, l = 10)),
      axis.text.x = element_text(size = 21),
      axis.text.y = element_text(size = 23),
      strip.text.x = element_text(size = 18),
      panel.spacing.x = unit(0.35, "in"),
      panel.spacing.y = unit(0.2, "in"),
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.spacing.x = unit(0.17, "in"),
      legend.title = element_text(face = "bold", size = 12),
      legend.text = element_text(face = "bold", size = 10),
      legend.key.width = unit(0.2, "in"),
      legend.key.height = unit(0.3, "in"),
      legend.text.align = 0
    ) +
    geom_vline(data = true.line, aes(xintercept = mean.hat))
}


breaks_fun <- function(x) {
  if (max(x) > 1) {
    c(0.4, 0.7, 1)
  } else if (min(x) < -2.9) {
    c(-3, -2.4, -1.6)
  } else if (min(x) < -1.9) {
    c(-2, -1.4, -0.8)
  } else if (min(x) < -0.9) {
    c(-1, -0.7, -0.4)
  } else {
    c(-0.4, -0.1, 0.2)
  }
}
