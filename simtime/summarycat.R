
# set working directory to the folder simtime---------------------------------------------------
setwd("C:/Users/agnes/Desktop/phd-thesis/my-projects/mixgb-paper/last-version/supplement/simtime")

library(stringr)
library(ggplot2)
library(grid)
library(dplyr)

source("sum_simtime.R")
source("plot_simtime.R")


# load result
# load categorical() result
n1e3p10 <- readRDS(file = "result/categorical/n1e3p10.rds")
n1e3p30 <- readRDS(file = "result/categorical/n1e3p30.rds")
n1e3p50 <- readRDS(file = "result/categorical/n1e3p50.rds")


n1e4p10 <- readRDS(file = "result/categorical/n1e4p10.rds")
n1e4p30 <- readRDS(file = "result/categorical/n1e4p30.rds")
n1e4p50 <- readRDS(file = "result/categorical/n1e4p50.rds")


n1e5p10 <- readRDS(file = "result/categorical/n1e5p10.rds")
n1e5p30 <- readRDS(file = "result/categorical/n1e5p30.rds")
n1e5p50 <- readRDS(file = "result/categorical/n1e5p50.rds")


n1e6p10 <- readRDS(file = "result/categorical/n1e6p10.rds")
n1e6p30 <- readRDS(file = "result/categorical/n1e6p30.rds")
n1e6p50 <- readRDS(file = "result/categorical/n1e6p50.rds")


# add columns of n, p
n1e3p10.df <- sum_simtime(n1e3p10)
n1e3p30.df <- sum_simtime(n1e3p30)
n1e3p50.df <- sum_simtime(n1e3p50)

n1e4p10.df <- sum_simtime(n1e4p10)
n1e4p30.df <- sum_simtime(n1e4p30)
n1e4p50.df <- sum_simtime(n1e4p50)

n1e5p10.df <- sum_simtime(n1e5p10)
n1e5p30.df <- sum_simtime(n1e5p30)
n1e5p50.df <- sum_simtime(n1e5p50)

n1e6p10.df <- sum_simtime(n1e6p10)
n1e6p30.df <- sum_simtime(n1e6p30)
n1e6p50.df <- sum_simtime(n1e6p50)

combine.df <- rbind(
  n1e3p10.df, n1e3p30.df, n1e3p50.df,
  n1e4p10.df, n1e4p30.df, n1e4p50.df,
  n1e5p10.df, n1e5p30.df, n1e5p50.df,
  n1e6p10.df, n1e6p30.df, n1e6p50.df
)


blues <- c("#00ddff", "#0160c9", "#020da5")
# mice.cart
yellows <- c("#fff701", "#c9ab01", "#a57901")
# mice.green
greens <- c("#88ff00", "#36be32", "#009353")
# mixgb.cpu
pinks <- c("#ff9fe5", "#db67ce", "#c341bf")
# mixgb.gpu
reds <- c("#ff97a5", "#db505f", "#c32030")

colors <- c(
  blues[2],
  greens[2],
  pinks[2],
  reds[2],
  pinks[3],
  reds[3]
)





plot_simtime(data = combine.df, color = colors, compare = "all", log.time = T, facet.samples = T)

p1 <- plot_simtime(data = combine.df, color = colors, compare = "all", log.time = T, facet.samples = T)


dir.folder <- "C:/Users/agnes/Desktop/phd-thesis/my-projects/mixgb-paper/last-version/jcgs"
jpeg(
  filename = file.path(dir.folder, "figures/timecat.jpeg"),
  width = 15, height = 6, units = "in", res = 300, pointsize = 1
)
grid.draw(p1, recording = T)
dev.off()
