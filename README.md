# supplement
This folder contains all the supplementary materials for the "mixgb: multiple imputation through XGBoost" paper <a href='http://arxiv.org/abs/2106.01574'>arXiv:2106.01574</a>.

In addition to the R code for the simulation study, we provide R scripts for generating all the tables and figures in this paper. 

# file: mixgb_0.1.1.tar.gz
This is the R package **mixgb** version 0.1.1, which is used for all simulations and computational experiments in this paper. 

Alternatively, users can download this version from CRAN or via 
> devtools::install_github("agnesdeng/mixgb@mixgb-paper").

Please note that the settings of future versions of the package may be changed on CRAN and Github. To reproduce the results of this paper, please use the bundled package included in the supplement folder.


## folder: Doove
This folder includes simulations code and results based on Doove (2014). This is provided for Reviewer 1 as requested. Since we re-designed our simulation study as suggested by Reviewer 2, we decided not to use this simulation in our revised paper. If Reviewer 1 agrees, we would like to remove this from our supplementary for consistency in the final version.

## file: PMM.R
The R script **PMM.R** is used to generate Figure 1 in Section 2.

## folder: simulation
R scripts for Section 3. For more details, please check the README.md file inside this folder.
  
## folder: simtime
This folde includes R code for evaluating the computational time for continuous and categorical data using simulated data in Section 4. For more details, please check the README.md file inside this folder.

## folder: datatime
This folde includes R code for evaluating the computational time for real datasets using simulated data in Section 4. For more details, please check the README.md file inside this folder.

## file: NWTS.R
The R script **NWTS.R** is used to generate Table 4, Figure 6, Figure 7, and Figure 8 in Section 5.


###Reference
Doove, Lisa L., Stef Van Buuren, and Elise Dusseldorp. "Recursive partitioning for missing data imputation in the presence of interaction effects." Computational statistics & data analysis 72 (2014): 92-104.
