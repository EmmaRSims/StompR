---
output:
  pdf_document: default
  html_document: default
---

# StompR

## Introduction
Statistical Comparison Package in R (StompR) is a tool for analysing the performance of various statistical methods against either a single dataset, or multiple datasets with the same response factor.

## Table of Contents
- [Introduction](#introduction)
- [Workflow](#workflow)
- [Installation](#installation)
  - [Dependencies](#dependencies)
  - [Devtools](#installing-from-devtools)
  - [GitHub](#installing-from-repository)
- [How to Use](#how-to-use)
  - [File Permissions](#file-permissions)
  - [Data Preparation](#data-preparation)
  - [Log Transform](#log-transform)
  - [Data Scaling](#data-scaling)
  - [Statistical Methods](#statistical-methods)
    - [Ordinary Least Squares Regression](#ordinary-least-squares-regression)
    - [Stepwise Linear Regression](#stepwise-linear-regression)
    - [Principal Components Regression](#principal-components-regression)
    - [Partial Least Squares Regression](#partial-least-squares-regression)
    - [Random Forest Regression](#random-forest-regression)
    - [Support Vector Machine](#support-vector-machine)
    - [K-Nearest Neighbours](#k-nearest-neighbours)
    - [Generalised Boosted Regression](#generalised-boosted-regression)
    - [Robust Linear Regression](#robust-linear-regression)
  - [General Parameters](#general-parameters)
- [Output](#output)
  - [Plots](#plots)
  - [Heatmap](#heatmap)
  
  
## Workflow

![Workflow](./images/Workflow.png?raw=true)


## Installation

### Dependencies
StompR was tested on and requires the following:
- **R** (>=3.6.1) 
- **These R Libraries:**
````
    pls (>= 2.7-2)    randomForest (>= 4.6-14)    e1071 (>= 1.7-3)
    FNN (>= 1.1.3)    robustbase (>= 0.93-5)      gbm (>= 2.1.5)
    ggbiplot (>= 0.55)
````
**Note:** This package also relies on the core `stats` package
          `ggbiplot` can be installed using devtools as it is not in the CRAN repository

### Installing from Devtools
One of the easiest ways to install packages from GitHub is through another package, `devtools`.
````
install.packages("devtools")
library(devtools)
install_github(repo = "EmmaRSims/StompR", dependencies = TRUE)
library(StompR)
````

### Installing from Repository
Alternatively, if you have Git installed, it is possible to clone this repository from the operating system CLI:
````
git clone https://github.com/EmmaRSims/StompR
````
Then, in R, it can be installed from the source:
````
library(utils)
install.packages("./StompR/", repos = NULL, type = "source")
````

## How to Use 

### File Permissions
The first parameter for the package is `file_path`, this should be a pre-existing folder on the system; it will be used for storing any and all plots generated within this package.
This package will require the ability to create and store files with the purpose of creating plots and storing them as PNGs. Before processing any data, the package will request explicit user consent to create these files. This can be bypassed/automated by setting the parameter `permission` to `TRUE`. 

### Data Preparation
There are two required input parameters for data within this package. The first, `yVector`, is the dependant factor  that is to be modelled; this must be a `numeric vector` class.
For `startStomping()`, the second input is `xMatrix`, which must be a `matrix` class where the columns are the continuous independant factors; this package assumes that the order of samples in `yVector` matches the order of samples in `xMatrix`, and to that end the number of rows in `xMatrix` must match the length of `yVector`.
In the case of multiple datasets, use the function `startStompingMultiple()`; the parameter `xMatrix` is replaced by `xMatrices` which is required to be a list of matrices where the columns are the continuous independant factors. Like `xMatrix`, each matrix in the list is assumed to have the same order of samples as `yVector` and must also have the same number of rows per matrix as the length of `yVector`. If the matrices withing the `xMatrices` list are named, these will be used to label axes and folders to distinguish between datasets; if they are not named, they will be numbered in the same order as they appear in the list.

### Log Transform
Biological data will often require the independant factors to be tranformed via the common logarithm (log10) to linearise it against the dependant factor. The parameter `logV` is a Boolean option which will transform the `xMatrix` dataset using the `log10` function. Setting this value to `TRUE` in the function `startStompingMultiple` will apply that transformation to every matrix within the list.

### Data Scaling
Most independent continuous factors within a dataset have varying scales. This makes it difficult to model and can increase error; to combat that, data can be scaled in various ways. This package offers 7 different methods of scaling the xMatrix, as well as leaving the data alone. These methods are:
- `raw`       Leave the data scale as it is
- `centre`    Use the Mean-Center Normalisation formula
- `minmax`    Use the min-max Normalisation formula
- `meannorm`  Use the Mean-Centered Normalisation formula (also known as Range Scaling)
- `zscore`    Use the Z-Score Normalisation formula
- `pareto`    Use the Pareto Normalisation Formula
- `vast`      Use the Variable Stability Scaling Method
- `level`     Use the Level Normalisation Formula

When specified in `startStompingMultiple`, these transforms will be applied to each dataset individually as they are iterated through.
[scaleFormulas](./images/scaleFormulas.png?raw=true)

### Statistical Methods
The main aim of this package is to compare statistical model performances versus the same data to find the method with the least amount of cumulative error. There are 11 different statistical methods currently available, these are:
- Ordinary Least Squares Regression (OLSR)
- Stepwise Linear Regression - Both directions (SLR)
- Stepwise Linear Regression - Forwards direction (SLRf)
- Stepwise Linear Regression - Backwards direction (SLRb)
- Principal Components Regression (PCR)
- Partial Least Squares Regression (PLSR)
- Random Forest Regression (RFR)
- Support Vector Machine (SVM)
- K-Nearest Neighbours Regression (KNN)
- Generalised Boosted Modelling (GBM)
- Robust Linear Regression (RLR) 

Instead of typing out the names of these methods, this package use a numerical vector, `meth`, where each number corresponds to a particular method. By default, this package will iterate over all of them. Their numerical values are:
- 1  OLSR
- 2  SLR
- 3  SLRf
- 4  SLRb
- 5  PCR
- 6  PLSR
- 7  RFR
- 8  SVM
- 9  KNN
- 10 GBM
- 11 RLR

#### Ordinary Least Squares Regression
This regression fits a line of best fit which reduces the total sum of errors, which is the total distance between the sample points and the line itself.

#### Stepwise Linear Regression
Is a selection method which starts with all the available predictors, and reduces the number of factors until an optimal model is achieved. These are usually built using a series of F-Tests to determine whether a factor should be kept or not. Forward direction stepwise regression involves adding factors to a blank model until it is at an optimal size, backwards stepwise regression involves starting with all factors and removing them until it is at an optimal performance, whereas both directions will do both adding and removing factors in an attempt to find the best combination.

#### Principal Components Regression
Principal components regression involves finding the covariance matrix and therefore the eigenvectors between factors to model the unobserved influences. This is also a good data dimension reduction technique. Using the value at the "elbow" of the scree plot will give a good idea about how many latent variables to include in the model, which is also the `plsr_ncomp` input variable.

#### Partial Least Squares Regression
This method looks at reducing the dimension of data via the covariance matrix, and then fits an ordinary least squares regression to these components.

#### Random Forest Regression


#### Support Vector Machine

#### K-Nearest Neighbours

#### Generalised Boosted Regression

#### Robust Linear Regression


### General Parameters
There are several general parameters in both `startStomping()` and `startStompingMultiple()`, these are:
- prop
- seed
- iter

`prop` is the parameter which sets the proportion of rows of `xMatrix` to build the model with. For example, `prop = 0.6` will use 60% of `xMatrix` to build the model, and calculate the MAPE and RMSE with the remaining 40% of the dataset; by default, `prop = 0.7`.
`seed` is used to create reproducable results from one simulation to the next. Mainly it is used to generate the indices for splitting the `xMatrix` into its respective training and testing datasets. 
`iter` is how many training datasets to generate and test each statistical method with; for example setting `iter = 10` will create 10 training sets, 10 models per statistical method, and 10 different RMSE values. The cumulative average per iteration is then calculated, resulting in a converged RMSE and MAPE value, by which the general performance and accuracy of that statistical method can be assessed.

##Output
`startStomping()` will return a list consisting of:
- `RMSE_CM`   The final cumulative mean value of the RMSE per statistical method
- `MAPE_CM`   The final cumulative mean value of the MAPE per statistical method
- `rmse_raw`  A matrix of raw RMSE values per model generated, where the row number is the iteration and the column number is the statistical method
- `mape_raw`  A matrix of raw MAPE values per model generated, where the row number is the iteration and the column number is the statistical method
- `models`    A list of all the models generated within the simulation for further analysis if desired

Whereas `startStompingMultiple()` will return a list of:
- `heatmap_dataframe`  A dataframe containing 4 columns:
  - `heatmap_methods`  The Statistical Methods
  - `heatmap_matrix`   The Dataset Names
  - `heatmap_values_mape` The corresponding MAPE value for the statistical method and dataset name at the same index in `heatmap_methods` and `heatmap_matrix`
  - `heatmap_values_rmse` The corresponding RMSE value for the statistical method and dataset name at the same index in `heatmap_methods` and `heatmap_matrix`
- `best_models` This is a list of the models with the lowest RMSE of all the iterations for each statistical method for each dataset


### Plots
Performance plots will be generated for both of the main functions. There will be a line plot per dataset to display:
- The Final Performance line plots for both the MAPE and RMSE
- A multiplot of line graphs showing the cumulative mean of both the RMSE and MAPE for each statistical method
- A multiplot of line graphs showing the raw value of both the RMSE and MAPE over each iteration for each statistical method

### Heatmap 
If comparing multiple datasets, it makes more sense to visualise the results with a heatmap which plots the dataset versus the statistical method. Two heatmaps are produced, one for each the RMSE and the MAPE. The colour of the square is dependant on the value of the respective error measurement, where red is poor and green is a good performance. The names of each dataset in the list `xMatrices` are used for the x axis, which if left blank will just be the index of the dataset in the list.

