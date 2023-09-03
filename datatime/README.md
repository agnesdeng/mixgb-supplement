#Folder: datatime
This folder contains R code for evaluating the computational time for real datasets in Section 4.

##File: summary.R
The R script `summary.R` is used to generate Table 3. Users need to set the working directory to `..../supplement`. 
It will load all results in the subfolder `result` to generate Table 3.

##File: result.R
This folder contains the computational time records for each dataset.

##Folder: allstate
This folder contains the R script `allstate.R` to record the computational time for the Allstate dataset. Since the dataset is large, we do not include it here. Users can download the Allstate data (train.csv) from the Kaggle website: https://www.kaggle.com/competitions/allstate-claims-severity/data. 

The dataset should be placed in the `allstate` folder.


##Folder: higgs
This folder contains the R script `higgs1M.R` to record the computational time for the `higgs1M` dataset. Since the dataset is large, we do not include it here. Users can download the higgs data (HIGGS.csv) from UCI Machine Learning Repository: https://archive.ics.uci.edu/dataset/280/higgs

The dataset should be placed in the `higgs` folder.


##Folder: credit
This folder contains the R script `credit.R` to record the computational time for the `credit` dataset. Since the dataset is not big, we include it here. Users can also download the credit data from UCI Machine Learning Repository: https://archive.ics.uci.edu/dataset/350/default+of+credit+card+clients

The dataset should be placed in the `credit` folder.
