# create categorical data (with n_levels) and a binary response for measure time performance
# data with n observations + (p categorial variables+1 binary response)
# all columns have roughly balanced class
create_cat<- function(n_obs = 1000,p=10,n_levels=3) {
  ratio<-n_levels-1
  z<-replicate(p,sample(0:(n_levels-1),size=n_obs,prob=rep(1/n_levels,n_levels),replace=T))
  colnames(z)<-paste0(rep("cat",p),1:p)
  coefs<-rep(c(-1/ratio,1/ratio),p/2)
  logit<-z %*% coefs
  prob<-1/(1+exp(-logit))
  y<-rbinom(n=n_obs,size=1,prob=prob)
  full.df<-data.frame(z,y)
  full.df<-full.df %>%
    mutate(across(contains(c("cat","y")),as.factor))
  full.df
}
