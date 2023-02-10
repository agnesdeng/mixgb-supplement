# supplement
This folder contains all supplementary materials for the paper "Multiple Imputation Through XGBoost" <a href='http://arxiv.org/abs/2106.01574'>arXiv:2106.01574</a>.

In addition to the R code for the simulation study, we provide R scripts for generating all the tables and figures in this paper. 

# File: mixgb_1.0.1.tar.gz
This is the R package **mixgb** version 1.0.1, which is used for all simulations and computational experiments in this paper. 

Alternatively, users can download this version via 
> devtools::install_github("agnesdeng/mixgb@mixgb-paper").

Please note that the settings of future versions of the package may be changed on CRAN and Github. To reproduce the results of this paper, please use the bundled package included in this supplement folder.


## File: PMM.R
The R script **PMM.R** is used to generate Figure 2 in Section 2.

## Folder: simulation
R scripts for Section 3. For more details, please refer to the README.md file inside this folder.

## Folder: datatime
This folde contains R code for evaluating the computational time for real datasets in Section 4. For more details, please refer to the README.md file inside this folder.
  
## Folder: simtime
This folde contains R code for evaluating the computational time for continuous and categorical data using simulated data in Section 4. For more details, please refer to the README.md file inside this folder.


## file: NWTS.R
The R script **NWTS.R** is used to create Table 4, Figure 7, Figure 8, and Figure 9 in Section 5.


