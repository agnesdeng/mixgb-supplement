#Folder: simulation

This folder contains R scripts for Section 3.

## File: summary.R
The R script `summary.R` is used to generate Figure 3, Figure 4, and Table 2 in Section 3.

It will load all simulation results in subfolder `result` and source plotting functions in subfolder `functions` to generate the outputs.

## Subfolder: data
- data.R
  This R script is used to generate a full complete dataset using the data generation model 3.1. Users need to set their working directory accordingly as this R script will source `create_mix.R` in this folder.
  
- create_mix.R
  This R script contains the function we need to generate the data.
  
- mar_mix.R
  This R script contains the function for generating missing values under a MAR mechanism as specified in Section 3.1

- full.rds 
  The full dataset we generated. 
  
## Subfolder: functions
These functions are used for summarizing and plotting simulation results. 

## Subfolder: result
All simulation results were saved in this folder. 

Note: The simulation functions (sim_cart.R; sim_mice.R; sim_mixgb.R; sim_rf.R) are designed to save simulation results every 100 runs. This was convenient as we could check the intermediate results. Users can also adapt these functions to make them return results every N runs. In the `supplement\simulation\result` folder, we deleted other intermediate results and only keep the final results (1000 runs). 

## Subfolder: sim
We use separate R scripts for running simulations for each MI method so that they can be run in parallel on the server. Users only need to run R scripts in the `simulation\sim\main` folder to obtain simulation results. Results will be saved in the folder `simulation\result`

##Files: sim_cart.R; sim_mice.R; sim_mixgb.R; sim_rf.R
In these functions, we set `r %% 100 ==0`, so that for every 100 runs, simulation results would be saved and written to the `result` folder.

### Sub-subfolder:  main
For each R script, users need to set the working directory to the `..../supplement/simulation`. 
R scripts in this folder will then source functions and data from other folders in the supplement folder.

- cartmaxit5.R
  mice-cart with maxit=5

- micemaxit5.R
  mice-default with maxit=5
  
- mixgbmaxit1.R
  mixgb with maxit=1
  
- mixgbmaxit1s.R
  mixgb-sub with maxit=1
  
- rfmaxit5.R
  mice-ranger with maxit=1
