
plot_simtime <- function(data, color = colors, compare = "all", log.time = F, facet.samples = T) {
  data <- data %>%
    dplyr::rename(Methods = expr) %>%
    dplyr::mutate(
      Methods = factor(Methods,
        levels = c(
          "mice.default",
          "mice.rf",
          "mixgb.cpu",
          "mixgb.gpu"
        )
      ),
      n_obs = factor(n_obs),
      n_p = factor(n_p)
    )

  data$Methods <- dplyr::recode_factor(data$Methods,
    mice.default = "mice-default",
    mice.rf = "mice-ranger",
    mixgb.cpu = "mixgb-cpu",
    mixgb.gpu = "mixgb-gpu"
  )

  if (log.time) {
    data <- data %>% dplyr::mutate(time = log(mean))
    y.titles <- "Log time (seconds)"
  } else {
    data <- data %>% dplyr::mutate(time = mean)
    y.titles <- "Time (seconds)"
  }

  shapes <- c(21, 22, 24, 25)
  lines <- c("dotted", "dashed", "longdash", "solid")
  if (facet.samples) {
    ggplot(data, aes(x = n_p, y = time, group = Methods, shape = Methods, linetype = Methods)) +
      geom_line(aes(colour = Methods, linetype = Methods), size = 1) +
      geom_point(aes(colour = Methods, shape = Methods, fill = Methods), size = 3) +
      facet_grid(~n_obs) +
      ylab(y.titles) +
      xlab("Number of features") +
      scale_color_manual(values = colors) +
      scale_fill_manual(values = colors) +
      scale_shape_manual(values = shapes) +
      scale_linetype_manual(values = lines) +
      scale_x_discrete(labels = c(
        "10 features" = "10", "30 features" = "30",
        "50 features" = "50"
      )) +
      theme(
        axis.title.x = element_text(face = "bold", size = 16),
        axis.title.y = element_text(face = "bold", size = 16),
        axis.text.x = element_text(face = "bold", size = 14),
        axis.text.y = element_text(face = "bold", size = 14),
        strip.text = element_text(face = "bold", size = 16), # change facet text size
        legend.position = "right",
        legend.key.size = unit(2, "cm"), # change legend key size
        legend.key.height = unit(2, "cm"), # change legend key height
        legend.key.width = unit(2, "cm"), # change legend key width
        legend.title = element_text(face = "bold", size = 18), # change legend title font size
        legend.text = element_text(face = "bold", size = 16)
      )
  } else {
    # facet=="features"
    ggplot(data, aes(x = n_obs, y = time, group = Methods, shape = Methods, linetype = Methods)) +
      geom_line(aes(colour = Methods, linetype = Methods), size = 1) +
      geom_point(aes(colour = Methods, shape = Methods, fill = Methods), size = 3) +
      facet_grid(~n_p) +
      ylab(y.titles) +
      xlab("Number of observations") +
      scale_color_manual(values = colors) +
      scale_fill_manual(values = colors) +
      scale_shape_manual(values = shapes) +
      scale_linetype_manual(values = lines) +
      scale_x_discrete(labels = c(
        "1e3 samples" = "1e3", "1e4 samples" = "1e4",
        "1e5 samples" = "1e5", "1e6 samples" = "1e6"
      ))
  }
}
