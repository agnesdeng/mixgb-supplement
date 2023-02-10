#Folder: simtime
R code for evaluating the computational time for continuous and categorical data using simulated data in Section 4.

##File: summarycon.R
The R script `summarycon.R` is used to generate Figure 4. Users need to set the working directory to `..../supplement/simtime`. 
It will source the `sum_simtime.R` and `plot_simtime.R` scripts and load all results in the subfolder `result` to generate Figure 4.

##File: summarycat.R
The R script `summarycat.R` is used to generate Figure 5. Users need to set the working directory to `..../supplement/simtime`. 
It will source the `sum_simtime.R` and `plot_simtime.R` scripts and load all results in the subfolder `result` to generate Figure 5.

## Files: sum_simtime.R
## Files: plot_simtime.R
These functions will be used by `summarycon.R` and `summarycat.R`. 

## Files: shellcategorical.R
## Files: shellcontinuous.R
These R scripts are used for running R scripts sequentially in Windows for computational time experiments. 

## Folder: categorical
This folder contains R scripts for computational time experiments on categorical data. Users need to set the working directory in each R script to `supplement/simtime`. Results will be saved in the folder `result`.

## Folder: continuous
This folder contains R scripts for computational time experiments on continuous data. Users need to set the working directory in each R script to `supplement/simtime`. Results will be saved in the folder `result`.

## Folder: data
This folder contains R scripts for generating simulated datasets for the experiment.
