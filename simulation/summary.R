
# set working directory to the folder simulation---------------------------------------------------

setwd("C:/Users/agnes/Desktop/phd-thesis/my-projects/mixgb-paper/last-version/supplement/simulation")

# load packages ------------------------------------------------------------

library(tibble)
library(dplyr)
library(tidyr)
library(ggplot2)
library(xtable)
library(plotrix)
library(grid)

# source functions for reporting and plotting-------------------------------------------

source("functions/report_coef.R")
source("functions/report_error.R")
source("functions/report_time.R")
source("functions/plot_coef.R")
source("functions/plot_var.R")



# load simulation results ------------------------------------------------------

mice.maxit5 <- readRDS(file = "result/mice/runs1000.rds")
cart.maxit5 <- readRDS(file = "result/cart/runs1000.rds")
rf.maxit5 <- readRDS(file = "result/rf/runs1000.rds")
mixgb.maxit1 <- readRDS(file = "result/mixgb/runs1000.rds")
mixgb_sub.maxit1 <- readRDS(file = "result/mixgbs/runs1000.rds")

## combine results
all.output <- list(
  "mice.maxit5" = mice.maxit5,
  "cart.maxit5" = cart.maxit5,
  "rf.maxit5" = rf.maxit5,
  "mixgb.maxit1" = mixgb.maxit1,
  "mixgb_sub.maxit1" = mixgb_sub.maxit1
)



# report ------------------------------------------------------------------
coef.sum <- report_coef(sim.output = all.output)
coef.sum

# print out latex table
multi_xtable(
  coef.sum[[1]], coef.sum[[2]], coef.sum[[3]], coef.sum[[4]], coef.sum[[5]],
  coef.sum[[6]], coef.sum[[7]], coef.sum[[8]], coef.sum[[9]], coef.sum[[10]], coef.sum[[11]], coef.sum[[12]],
  coef.sum[[13]], coef.sum[[14]]
)
###
multi_xtable(coef.sum[[1]], coef.sum[[2]], coef.sum[[3]])
multi_xtable(coef.sum[[4]], coef.sum[[5]], coef.sum[[6]])
multi_xtable(coef.sum[[7]], coef.sum[[8]], coef.sum[[9]])
multi_xtable(coef.sum[[10]], coef.sum[[11]], coef.sum[[12]])
multi_xtable(coef.sum[[13]], coef.sum[[14]])


# complete.cases: bias
tibble(
  cp.bias = all.output[[1]]$cp.coefs - all.output[[1]]$hat.coefs,
  estimates = factor(rep(all.output[[1]]$estimates, length(all.output[[1]]$sim.time)),
    levels = all.output[[1]]$estimates
  )
) %>%
  dplyr::group_by(estimates) %>%
  dplyr::summarise(Bias = mean(cp.bias))


set.seed(2022)
# maximum mc errors for bias, coverage, ci width, missinfo
mcerrors <- mce_rrors(sim.output = all.output)
mcerrors

# bootstrap error for variance
booterror <- boot_error(sim.output = all.output, boot.runs = 100)
booterror


# winner: least biased estimates
Methods <- c("mice-default", "mice-cart", "mice-rf", "mixgb", "mixgb-sub")
bias.result <- data.frame(estiamtes = all.output[[1]]$estimates, winner = NA)
for (i in 1:14) {
  bias.result$winner[i] <- Methods[which.min(abs(coef.sum[[i]]$Bias))]
}
bias.result


# summary of computational time for simulation study
report_time(sim.output = all.output)




# plot MI coefficient
plot_coef(sim.output = all.output, show.vertical = F)
# plot imputation variance
plot_var(sim.output = all.output, show.vertical = F)


# save figures
dir.path <- "C:/Users/agnes/Desktop/phd-thesis/my-projects/mixgb-paper/last-version/jcgs/figures"
p1 <- plot_coef(sim.output = all.output, show.vertical = F)
jpeg(
  filename = file.path(dir.path, "/coefs.jpeg"),
  width = 15, height = 8, units = "in", res = 300, pointsize = 1
)
grid.draw(p1, recording = T)
dev.off()



p1 <- plot_var(sim.output = all.output, show.vertical = F)
jpeg(
  filename = file.path(dir.path, "/var.jpeg"),
  width = 15, height = 8, units = "in", res = 300, pointsize = 1
)
grid.draw(p1, recording = T)
dev.off()
