# create simulated data ----------------------------------------------
create_mix <- function(n_obs = 1000,binordAsFactor=TRUE) {
  
  # normal data -------------------------------------------------------------
  # covariance matrix
  cov.m <- matrix(0, ncol = 4, nrow = 4)
  cov.m[1:4, 1:4] <- 0.6
  diag(cov.m) <- 1
  
  # multivariate normal(4 variables)
  z <- mvrnorm(n = n_obs, mu = rep(0, 4), Sigma = cov.m, empirical = TRUE)
  
  norm1<-z[,1]
  norm2<-z[,2]
  norm3<-z[,3]
  norm4<-z[,4]
  
  
  # correlated continuous and binary variable -------------------------------
  #cor(norm5,bin1)=0.54
  #cor(norm6,bin1)=0.56
  #cor(norm5,norm6)= 0.7
  cov.m <- matrix(0, ncol = 3, nrow = 3)
  cov.m[1:3, 1:3] <- 0.7
  diag(cov.m) <- 1
  z <- mvrnorm(n = n_obs, mu = rep(0, 3), Sigma = cov.m, empirical = TRUE)
  u <- pnorm(z)
  norm5 <- qnorm(p = u[, 1], mean = 0, sd = 1)
  norm6 <- qnorm(p = u[, 2], mean = 0, sd = 1)
  bin1<- qbinom(p = u[, 3], size = 1, prob = 0.5)

  
  
  # correlated continuous and ordinal variable -------------------------------
  #cor(norm7,ord1)=0.62
  #cor(norm8,ord1)=0.63
  #cor(norm7,norm8)= 0.7
  cov.m <- matrix(0, ncol = 3, nrow = 3)
  cov.m[1:3, 1:3] <- 0.7
  diag(cov.m) <- 1
  z <- mvrnorm(n = n_obs, mu = rep(0, 3), Sigma = cov.m, empirical = TRUE)
  u <- pnorm(z)
  norm7 <- qnorm(p = u[, 1], mean = 0, sd = 1)
  norm8 <- qnorm(p = u[, 2], mean = 0, sd = 1)
  ord1 <- qbinom(p = u[, 3], size =2 , prob = 0.5)
  
  

  norm.names <- paste0("norm", 1:8)
  bin.names <- paste0("bin", 1)
  ord.names <- paste0("ord", 1)

  full.tb <- data.frame(mget(c(norm.names, bin.names, ord.names)))

  
  full.tb <- full.tb %>% 
    dplyr::mutate(y.num = norm1 + norm2  + norm3 +
                    norm5+
                    norm7+
                    bin1+
                    -ord1+
                    norm1^2 + norm2*norm3+
                    -3*norm5*bin1-2*norm7*(ord1==1)+norm7*(ord1==2)
                    +rnorm(n = n_obs, mean = 0, sd = 1))
  
  if(binordAsFactor){
    full.tb %>%
      dplyr:: mutate(across(contains(c("bin","ord")),as.factor))
  }else{
    full.tb
  }
}


