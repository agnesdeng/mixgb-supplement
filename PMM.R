
library(mixgb)
library(ggplot2)
# Data generation ---------------------------------------------------------

create.data <- function(beta = 5, sigma = 1, Nrow = 1000, run = 1) {
  set.seed(seed = run)
  x <- rnorm(Nrow, mean = 0, sd = 1)
  y <- beta * x + rnorm(Nrow, mean = 0, sd = sqrt(sigma))
  sim.df <- data.frame(x = x, y = y)
  return(sim.df)
}


# MCAR
mcar.data <- function(complete.df, p = 0.5) {
  x.miss <- rbinom(nrow(complete.df), 1, p)
  complete.df$x[x.miss == 1] <- NA
  complete.df
}

# obtain a set of data with missing values
set.seed(2022)
complete.df <- create.data(Nrow = 1000)
withNA.df <- mcar.data(complete.df, p = 0.5)


# mixgb
M <- 5
mixgb.noPMM <- mixgb(data = withNA.df, m = M, bootstrap = FALSE, pmm.type = NULL)
mixgb.PMM1 <- mixgb(data = withNA.df, m = M, bootstrap = FALSE, pmm.type = 1)
mixgb.PMM2 <- mixgb(data = withNA.df, m = M, bootstrap = FALSE, pmm.type = 2)




idx <- which(is.na(withNA.df$x))
N <- length(idx)
ON <- length(withNA.df$x) - N

y <- c(complete.df[-idx, "y"], rep(complete.df[idx, "y"], 4))
x <- c(complete.df[-idx, "x"], complete.df[idx, "x"], mixgb.noPMM[[1]][idx, x], mixgb.PMM1[[1]][idx, x], mixgb.PMM2[[1]][idx, x])
type <- factor(c(rep("observed", ON), rep(c("masked true", "mixgb (no pmm)", "mixgb (pmm.type=1)", "mixgb (pmm.type=2)"), each = N)),
  levels = c("observed", "masked true", "mixgb (no pmm)", "mixgb (pmm.type=1)", "mixgb (pmm.type=2)")
)
combine.df <- data.frame(x = x, y = y, type = type)


p <- ggplot(combine.df, aes(x = x, y = y, color = type)) +
  geom_point() +
  facet_grid(~type) +
  theme(
    strip.text.x = element_text(size = 16),
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 18)
  ) +
  theme(legend.position = "none") +
  scale_color_manual(values = c("gray40", "gray20", c("#ffa9f3", "#ed88e1", "#db67ce")))

p

dir.path <- "C:/Users/agnes/Desktop/mixgb-paper/jcgs"

jpeg(
  filename = file.path(dir.path, "figures/pmm.jpeg"),
  width = 12, height = 4, units = "in", res = 300, pointsize = 10
)
print(p)
dev.off()
