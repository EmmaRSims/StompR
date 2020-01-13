# StompR
Statistical Comparison Package in R (StompR) is a tool for analysing the performance of various statistical methods against either a single dataset, or multiple datasets with the same response factor.

## Table of Contents
- [StompR](#stompr)
- [Installation](#installation)
  - [Dependencies](#dependencies)
  - [Devtools](#installing-from-devtools)
  - [GitHub](#installing-from-repository)
- [How to Use](#how-to-use)
  - [Data Preparation](#data-preparation)
  - [File Permissions](#file-permissions)
  - [Input Data](#input-data)
  - [Data Transformations](#data-transformations)
  - [Statistical Methods](#statistical-methods)
  - [Error Analysis](#error-analysis)
  - [Output](#output)

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
### Data Preparation

### File Permissions
This package will require the ability to create and store files with the purpose of creating plots. For full transparency, the package will request explicit consent to create these files before any data is processed.
this can be bypassed/automated by setting the parameter `permission` to `TRUE`.

### Parameters
