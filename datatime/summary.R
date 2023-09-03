# set working directory to the folder supplement---------------------------------------------------
setwd("C:/Users/agnes/Desktop/phd-thesis/my-projects/mixgb-paper/v5/supplement")

library(dplyr)
library(plotrix)

# load result -------------------------------------------------------------

credit.time <- readRDS("datatime/result/credit0.rds")
credit.time

credit.sum <- readRDS("datatime/result/credit.rds")
credit.sum


higgs1M.time <- readRDS("datatime/result/higgs1M0.rds")
higgs1M.time

higgs1M.sum <- readRDS("datatime/result/higgs1M.rds")
higgs1M.sum


allstate.time <- readRDS("datatime/result/allstate0.rds")
allstate.time

allstate.sum <- readRDS("datatime/result/allstate.rds")
allstate.sum



# get summary -------------------------------------------------------------
ns_s <- 1e9

# credit
time <- credit.time$time / ns_s
methods <- credit.time$expr
result.df <- data.frame(methods = methods, time = time)

credit.summary <- result.df %>%
  dplyr::group_by(methods) %>%
  dplyr::summarise(Mean = mean(time), SE = std.error(time))
credit.summary

# allstate
time <- allstate.time$time / ns_s
methods <- allstate.time$expr
result.df <- data.frame(methods = methods, time = time)

allstate.summary <- result.df %>%
  dplyr::group_by(methods) %>%
  dplyr::summarise(Mean = mean(time), SE = std.error(time))
options(pillar.sigfig = 7)
allstate.summary

# higgs1M
time <- higgs1M.time$time / ns_s
methods <- higgs1M.time$expr
result.df <- data.frame(methods = methods, time = time)

higgs1M.summary <- result.df %>%
  dplyr::group_by(methods) %>%
  dplyr::summarise(Mean = mean(time), SE = std.error(time))
higgs1M.summary
