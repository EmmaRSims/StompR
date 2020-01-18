---
output:
  pdf_document: default
  html_document: default
---
# Introduction
Statistical Comparison Package in R (StompR) is a tool for analysing the performance of various statistical methods against either a single dataset, or multiple datasets with the same response factor.

## Table of Contents
- [Introduction](#introduction)
- [StompR](#stompr)
- [Workflow](#workflow)
- [Installation](#installation)
  - [Dependencies](#dependencies)
  - [Devtools](#installing-from-devtools)
  - [GitHub](#installing-from-repository)
- [How to Use](#how-to-use)
  - [File Permissions](#file-permissions)
  - [Data Preparation](#data-preparation)
    - [Single Dataset](#data-preparation-single-matrix)
    - [Multiple Datasets](#data-preparation-multiple-matrices)
  - [Log Transform](#log-transform)
  - [Data Transformations](#data-transformations)
  - [Statistical Methods](#statistical-methods)
  - [General Parameters](#general-parameters)
- [Output](#output)
  - [Plots](#plots)
  - [Heatmap](#heatmap)
  - [Models](#models)
  
##StompR
  
## Workflow


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
This package will require the ability to create and store files with the purpose of creating plots. For full transparency, the package will request explicit consent to create these files before any data is processed.
this can be bypassed/automated by setting the parameter `permission` to `TRUE`.
### Functions
There are currently two main functions, `startStomping()` and `startStompingMultiple()`. The purpose of `startStomping()` is to compare  
#### Data Preparation single matrix
There are only 3 required inputs, these are:
 - The file path to the folder in which the results will be stored
 - A matrix of dependent variables, where each column is a separate factor (must be a matrix class)
 - A vector for the independent variable which is being modelled (must be a numeric class)
The length of the vector and number of matrix rows must match. This package focuses on regression, so the data should be continuous rather than discrete (categorical).

#### Data Preparation (multiple matrices)
Similarly to the single matrix, there are only 3 required inputs; these are:
 - The file path to the folder in which the results will be stored
 - A list of matrices of dependent variables, where each column in the matrix is a separate factor (must be a list class of matrix classes)
 - A vector for the independent variable which is being modelled (must be a numeric class)
### Parameters
