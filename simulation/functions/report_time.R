
report_time <- function(sim.output, log.time = F, title = "Simulation time", colors = NULL) {
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

  time.long <- sim.output %>%
    lapply(function(x) x[["sim.time"]]) %>%
    as_tibble() %>%
    tibble::add_column(sim_id = 1:full.runs) %>%
    tidyr::pivot_longer(!sim_id, names_to = "Methods", values_to = "Time") %>%
    drop_na() %>%
    dplyr::mutate(
      Methods = factor(Methods, levels = names(sim.output)),
      Log.Time = log(Time)
    )
  if (log.time) {
    sum.time <- time.long %>%
      group_by(Methods) %>%
      summarise(
        mean.time = mean(Log.Time),
        sd.time = sd(Log.Time),
        se.time = std.error(Log.Time)
      )
  } else {
    sum.time <- time.long %>%
      group_by(Methods) %>%
      summarise(
        mean.time = mean(Time),
        sd.time = sd(Time),
        se.time = std.error(Time)
      )
  }


  sum.time
}
