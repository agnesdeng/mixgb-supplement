
# set working directory to the folder simtime/categorical---------------------------------------------------
setwd("C:/Users/agnes/Desktop/phd-thesis/my-projects/mixgb-paper/v5/supplement/simtime/categorical")


#Run the R script for n=1e3
shell("Rscript --no-save --no-restore ./n1e3.R")

#Run the R script for n=1e4
shell("Rscript --no-save --no-restore ./n1e4.R")

#Run the R script for n=1e5
shell("Rscript --no-save --no-restore ./n1e5.R")

#Run the R script for n=1e6
shell("Rscript --no-save --no-restore ./n1e6.R")




