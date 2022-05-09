# CellCheck <img src="Figures/CellCheck2.png" align="right" width="120" />
## A toolkit for validating single-cell analysis with visualization

## Overview

CellCheck is a toolkit for validating single-cell analysis with visualization.
You can manipulate three types of data with CellCheck: binary data, discrete Multiple data, and continuous data.


## Required software
CellCheck runs in the R statistical computing environment. You will need R version 3.6.3 or to have access to the latest features.


## Installation

```{r, eval = FALSE}
  ## Check whether the installation of those packages is required 
  Package.set <- c("tidyverse","caret","cvms","DescTools","devtools")
  for (i in 1:length(Package.set)) {
    if (!requireNamespace(Package.set[i], quietly = TRUE)){
      install.packages(Package.set[i])
    }
  }
  ## Load Packages
  # library(Seurat)
  lapply(Package.set, library, character.only = TRUE)
  rm(Package.set,i)

  ## install CellCheck
  # Install the CellCheck package
  detach("package:CellCheck", unload = TRUE)
  devtools::install_github("Charlene717/CellCheck")
  # Load CellCheck
  library(CellCheck)
```

## Usage

#### Load the Demo RData ####
```{r, eval = FALSE}
## Load simulation datafrme ##
  load("Create_simulation_datafrme.RData")
```
#### Run the Demo script ####
```{r, eval = FALSE}
## Load simulation datafrme ##
source("Demo_CellTypeAnno.R", echo = TRUE, max.deparse.length=10000, encoding="utf-8",
       print.eval = TRUE) 
```

## Export files
#### Binary data
<img src="https://github.com/Charlene717/CellCheck/blob/main/Figures/Binary_data.jpg">

#### Discrete multiple data
<img src="https://github.com/Charlene717/CellCheck/blob/main/Figures/Discrete_multiple_data.jpg">

#### Continuous data
<img src="https://github.com/Charlene717/CellCheck/blob/main/Figures/Continuous_data.jpg">


