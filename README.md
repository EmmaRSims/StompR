---
output:
  html_document: default
  pdf_document: default
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
  - [General Parameters](#general-parameters)
- [Output](#output)
  - [Plots](#plots)
  - [Heatmap](#heatmap)
  - [Models](#models)
  
  
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
````
**Note:** This package also relies on the core `stats` package

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


