library(addhazard)
library(mice)
library(mixgb)
library(dplyr)
library(mitools)
library(ggpubr)
library(grid)
library(xtable)
library(survival)

# Load the NWTS dataset from the "addhazard" package ----------------------
data(nwtsco)
str(nwtsco)
# the original data has no missing value
colSums(is.na(nwtsco))

# make changes to the data
nwts <- nwtsco

# change the following variables: age,stage
nwts$age1 <- with(nwts, pmin(age, 1))
nwts$age2 <- with(nwts, pmax(age, 1))

nwts$stage1 <- ifelse(nwts$stage > 2, 1, 0)

nwts <- nwts %>%
  dplyr::select(!c(age, stage))
str(nwts)


complete.df <- nwts



# MAR missing in "histol" depends on the response "relaps" and "instit"
mar_histol <- function(data = nwts) {
  na.prob <- data %>%
    mutate(
      na.prob = case_when(
        relaps == "0" & instit == "0" ~ 0.2,
        relaps == "1" & instit == "0" ~ 0.6,
        relaps == "0" & instit == "1" ~ 0.1,
        relaps == "1" & instit == "1" ~ 0.4,
      )
    ) %>%
    dplyr::select(na.prob) %>%
    pull()

  miss <- rbinom(nrow(data), 1, na.prob)
  data[, "histol"][miss == 1] <- NA
  data
}

# MAR missing in "stage1" depends on the response "relaps" and "instit"
mar_stage1 <- function(data = nwts) {
  na.prob <- data %>%
    mutate(
      na.prob = case_when(
        relaps == "0" & instit == "0" ~ 0.2,
        relaps == "1" & instit == "0" ~ 0.6,
        relaps == "0" & instit == "1" ~ 0.1,
        relaps == "1" & instit == "1" ~ 0.4,
      )
    ) %>%
    dplyr::select(na.prob) %>%
    pull()

  miss <- rbinom(nrow(data), 1, na.prob)
  data[, "stage1"][miss == 1] <- NA
  data
}

# MAR missing in "tumdiam" depends on the response "relaps" and "specwgt"
mar_tumdiam <- function(data = nwts) {
  na.prob <- data %>%
    mutate(
      na.prob = case_when(
        relaps == "0" & specwgt < 800 ~ 0.1,
        relaps == "1" & specwgt < 800 ~ 0.5,
        relaps == "0" & specwgt >= 800 ~ 0.5,
        relaps == "1" & specwgt >= 800 ~ 0.1
      )
    ) %>%
    dplyr::select(na.prob) %>%
    pull()

  miss <- rbinom(nrow(data), 1, na.prob)
  data[, "tumdiam"][miss == 1] <- NA
  data
}





# create data with missing values
set.seed(2022)

withNA.df <- mar_histol(data = nwts)
withNA.df <- mar_tumdiam(data = withNA.df)
withNA.df <- mar_stage1(data = withNA.df)

colSums(is.na(withNA.df))
sum(complete.cases(withNA.df))



#full data
model.form <- as.formula(Surv(trel, relaps) ~ histol + +histol:age1 + histol:age2 + age1 + age2 + stage1 + tumdiam + stage1:tumdiam)
full.fit <- coxph(model.form, data = nwts)
full.coefs <- coef(full.fit)
full.coefs

#complete cases
cp.fit <- coxph(model.form, data = withNA.df)
cp.coefs <- coef(cp.fit)
cp.coefs



# MI methods
m <- 5


# mice-default
mice.imp <- mice(data = withNA.df, printFlag = FALSE, m = m)
mice.data <- complete(mice.imp, action = "all")
mice.fit <- lapply(mice.data, function(dataset) coxph(model.form, data = dataset))
mice.coefs <- do.call(cbind, lapply(mice.fit, coef))
mice.coefs <- rowMeans(mice.coefs)
mice.coefs


# mice-cart
cart.imp <- mice(data = withNA.df, printFlag = FALSE, method = "cart", m = m)
cart.data <- complete(cart.imp, action = "all")

# bug in mice.cart: integer variables were imputed as numeric variabes, so we change the type of `tumdiam`, `histol` to integer for plotting.
cart.data <- lapply(cart.data, function(x) {
  x$tumdiam <- as.integer(x$tumdiam)
  x
})

cart.data <- lapply(cart.data, function(x) {
  x$histol <- as.integer(x$histol)
  x
})

cart.fit <- lapply(cart.data, function(dataset) coxph(model.form, data = dataset))
cart.coefs <- do.call(cbind, lapply(cart.fit, coef))
cart.coefs <- rowMeans(cart.coefs)
cart.coefs

# mice-rf
rf.imp <- mice(data = withNA.df, printFlag = FALSE, method = "rf", rfPackage = "ranger", m = m, num.threads = 1)
rf.data <- complete(rf.imp, action = "all")

rf.fit <- lapply(rf.data, function(dataset) coxph(model.form, data = dataset))
rf.coefs <- do.call(cbind, lapply(rf.fit, coef))
rf.coefs <- rowMeans(rf.coefs)
rf.coefs


# mixgb
xgb.params <- list(
  nthread = 1, max_depth = 3, gamma = 0, eta = 0.3, min_child_weight = 1, subsample = 1, colsample_bytree = 1, colsample_bylevel = 1, colsample_bynode = 1,
  tree_method = "auto"
)


mixgb.data <- mixgb(data = withNA.df, xgb.params = xgb.params, nrounds = 100, bootstrap = FALSE, m = m, maxit = 1)
mixgb.fit <- lapply(mixgb.data, function(dataset) coxph(model.form, data = dataset))
mixgb.coefs <- do.call(cbind, lapply(mixgb.fit, coef))
mixgb.coefs <- rowMeans(mixgb.coefs)
mixgb.coefs

# mixgb-sub
xgb.params <- list(
  nthread = 1, max_depth = 3, gamma = 0, eta = 0.3, min_child_weight = 1, subsample = 0.7, colsample_bytree = 1, colsample_bylevel = 1, colsample_bynode = 1,
  tree_method = "auto"
)


mixgb_sub.data <- mixgb(data = withNA.df, xgb.params = xgb.params, nrounds = 100, bootstrap = FALSE, m = m, maxit = 1)

mixgb_sub.fit <- lapply(mixgb_sub.data, function(dataset) coxph(model.form, data = dataset))
mixgb_sub.coefs <- do.call(cbind, lapply(mixgb_sub.fit, coef))
mixgb_sub.coefs <- rowMeans(mixgb_sub.coefs)
mixgb_sub.coefs

# final result
combine.coefs <- cbind(full.coefs, cp.coefs, mice.coefs, cart.coefs, rf.coefs, mixgb.coefs, mixgb_sub.coefs)
combine.coefs

#Table 4
xtable(combine.coefs)






#plots for imputed values -----------------------------------------------

blues <- c("#00ddff", "#0160c9", "#020da5")
# mice.cart
yellows <- c("#fff701", "#c9ab01", "#a57901")
# mice.green
greens <- c("#88ff00", "#36be32", "#009353")
# mixgb.cpu
pinks <- c("#ff9fe5", "#db67ce", "#c341bf")
# mixgb.gpu
reds <- c("#ff97a5", "#db505f", "#c32030")

mice.colors <- c("gray40", "gray20", rep(blues[2], 5))
cart.colors <- c("gray40", "gray20", rep(yellows[2], 5))
rf.colors <- c("gray40", "gray20", rep(greens[2], 5))
mixgb.colors <- c("gray40", "gray20", rep(pinks[2], 5))
mixgb_sub.colors <- c("gray40", "gray20", rep(reds[2], 5))

# #Figure 6:  mean imputation

single.imp <- withNA.df %>%
  mutate(tumdiam = ifelse(is.na(tumdiam), mean(tumdiam, na.rm = TRUE), tumdiam))

mean.data <- list()
for (i in 1:5) {
  mean.data[[i]] <- single.imp
}

# int was imputed as numeric
mean.data <- lapply(mean.data, function(x) {
  x$tumdiam <- as.integer(x$tumdiam)
  x
})



dir.path <- "C:/Users/agnes/Desktop/phd-thesis/my-projects/mixgb-paper/last-version/jcgs"

p1 <- plot_hist(
  imputation.list = mean.data, var.name = "tumdiam",
  original.data = withNA.df, true.data = nwts, color.pal = c("gray40", "gray20", rep("black", 5))
)

p1 <- p1+scale_y_continuous(breaks = c(0,0.25,0.5))+
  theme(
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    strip.text = element_text(size =18 ,face = "plain"),
    axis.title.x = element_text(size = 22, margin = margin(t = 10, r = 0, b = 0, l = 0), ),
    axis.title.y = element_text(size = 22, margin = margin(0, r = 5, 0, l = 0)),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
  )

jpeg(
  filename = file.path(dir.path, "figures/imputemean1.jpeg"),
  width = 15, height = 2.5, units = "in", res = 300, pointsize = 1
)

grid::grid.draw(p1, recording = T)
dev.off()



# Figure 7: impute by sampling
set.seed(2022)
mice.sample <- mice(data = withNA.df, printFlag = FALSE, m = 5, maxit = 5, method = "sample")
mice_sample.data <- complete(mice.sample, action = "all")

p1 <- plot_hist(
  imputation.list = mice_sample.data, var.name = "tumdiam",
  original.data = withNA.df, true.data = nwts, color.pal = c("gray40", "gray20", rep("black", 5))
)


p1 <- p1+
  theme(
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    strip.text = element_text(size = 18,face = "plain"),
    axis.title.x = element_text(size = 22, margin = margin(t = 10, r = 0, b = 0, l = 0), ),
    axis.title.y = element_text(size = 22, margin = margin(0, r = 5, 0, l = 0)),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
  )
jpeg(
  filename = file.path(dir.path, "figures/imputesample1.jpeg"),
  width = 15, height = 2.5, units = "in", res = 300, pointsize = 1
)
grid::grid.draw(p1, recording = T)
dev.off()

p2 <- plot_2num(
  imputation.list = mice_sample.data, var.x = "specwgt", var.y = "tumdiam",
  original.data = withNA.df, true.data = nwts, color.pal = c("gray40", "gray20", rep("black", 5))
)

p2 <- p2+
  scale_x_continuous(breaks = c(0,2000,4000))+
  theme(
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    strip.text = element_text(size = 18,face = "plain"),
    axis.title.x = element_text(size = 22, margin = margin(t = 10, r = 0, b = 0, l = 0), ),
    axis.title.y = element_text(size = 22, margin = margin(0, r = 5, 0, l = 0)),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
  )




jpeg(
  filename = file.path(dir.path, "figures/imputesample2.jpeg"),
  width = 15, height = 2.5, units = "in", res = 300, pointsize = 1
)
grid::grid.draw(p2, recording = T)
dev.off()


# Figure 8: impute by MI methods
p1 <- plot_2num(
  imputation.list = mice.data, var.x = "specwgt", var.y = "tumdiam",
  original.data = withNA.df, true.data = nwts, color.pal = mice.colors
)
p2 <- plot_2num(
  imputation.list = cart.data, var.x = "specwgt", var.y = "tumdiam",
  original.data = withNA.df, true.data = nwts, color.pal = cart.colors
)
p3 <- plot_2num(
  imputation.list = rf.data, var.x = "specwgt", var.y = "tumdiam",
  original.data = withNA.df, true.data = nwts, color.pal = rf.colors
)
p4 <- plot_2num(
  imputation.list = mixgb.data, var.x = "specwgt", var.y = "tumdiam",
  original.data = withNA.df, true.data = nwts, color.pal = mixgb.colors
)
p5 <- plot_2num(
  imputation.list = mixgb_sub.data, var.x = "specwgt", var.y = "tumdiam",
  original.data = withNA.df, true.data = nwts, color.pal = mixgb_sub.colors
)

p1 <- p1+
  scale_x_continuous(breaks = c(0,2000,4000))+
  theme(
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    strip.text = element_text(size = 18,face = "plain"),
    axis.title.x = element_text(size = 22, margin = margin(t = 10, r = 0, b = 0, l = 0), ),
    axis.title.y = element_text(size = 22, margin = margin(0, r = 5, 0, l = 0)),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
  )

p2 <- p2 +
  scale_x_continuous(breaks = c(0,2000,4000))+
  theme(
  plot.title = element_blank(),
  plot.subtitle = element_blank(),
  strip.text = element_text(size = 18,face = "plain"),
  axis.title.x = element_text(size = 22, margin = margin(t = 10, r = 0, b = 0, l = 0), ),
  axis.title.y = element_text(size = 22, margin = margin(0, r = 5, 0, l = 0)),
  axis.text.x = element_text(size = 18),
  axis.text.y = element_text(size = 18),
)

p3 <- p3 +
  scale_x_continuous(breaks = c(0,2000,4000))+
  theme(
  plot.title = element_blank(),
  plot.subtitle = element_blank(),
  strip.text = element_text(size = 18,face = "plain"),
  axis.title.x = element_text(size = 22, margin = margin(t = 10, r = 0, b = 0, l = 0), ),
  axis.title.y = element_text(size = 22, margin = margin(0, r = 5, 0, l = 0)),
  axis.text.x = element_text(size = 18),
  axis.text.y = element_text(size = 18),
)

p4 <- p4 +
  scale_x_continuous(breaks = c(0,2000,4000))+
  theme(
  plot.title = element_blank(),
  plot.subtitle = element_blank(),
  strip.text = element_text(size = 18,face = "plain"),
  axis.title.x = element_text(size = 22, margin = margin(t = 10, r = 0, b = 0, l = 0), ),
  axis.title.y = element_text(size = 22, margin = margin(0, r = 5, 0, l = 0)),
  axis.text.x = element_text(size = 18),
  axis.text.y = element_text(size = 18),
)

p5 <- p5 +
  scale_x_continuous(breaks = c(0,2000,4000))+
  theme(
  plot.title = element_blank(),
  plot.subtitle = element_blank(),
  strip.text = element_text(size = 18,face = "plain"),
  axis.title.x = element_text(size = 22, margin = margin(t = 10, r = 0, b = 0, l = 0), ),
  axis.title.y = element_text(size = 22, margin = margin(0, r = 5, 0, l = 0)),
  axis.text.x = element_text(size = 18),
  axis.text.y = element_text(size = 18),
)

combinescatter <- ggarrange(p1, p2, p3, p4, p5, nrow = 5)

jpeg(
  filename = file.path(dir.path, "figures/combinescatter.jpeg"),
  width = 15, height = 12.5, units = "in", res = 300, pointsize = 1
)
grid::grid.draw(combinescatter, recording = T)
dev.off()
