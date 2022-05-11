# CellCheck <img src="Figures/CellCheck2.png" align="right" width="120" />
## A toolkit for validating single-cell analysis with visualization
<br> 
<br> 
## Overview
CellCheck is a toolkit for validating single-cell analysis with visualization.
<br> You can use CellCheck to verify malignancy annotation, cell type annotation, scoring, and deconvolution results from the different conditions. For example, you can compare the performance of different scRNA-seq analysis tools, or different improved algorithms. You can also optimize the parameters used in these tools.
<br> 
<br> It can also be applied to other use. For example, it can be extended to find biomarkers or gene signatures from the database such as TCGA.
<br> 
<br> 
<br> 
<img src="https://github.com/Charlene717/CellCheck/blob/main/Figures/CellCheck_Overview.jpg">
<br> 
<br>


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
You can manipulate three types of data with CellCheck: binary data, multiple discrete data, and continuous data.
<br> You can try to load the following demo RData to run the whole function in CellCheck.
#### Load the Demo RData ####
```{r, eval = FALSE}
## Load simulation datafrme ##
  load("Create_simulation_datafrme3.RData")
```
#### Run the Demo script ####
```{r, eval = FALSE}
## Load simulation datafrme ##
source("Demo_CellTypeAnno.R", echo = TRUE, max.deparse.length=10000, encoding="utf-8",
       print.eval = TRUE) 
```

## Input and export files
### Binary data
<br> 
The format of input binary data can be number or character:
<img src="https://github.com/Charlene717/CellCheck/blob/main/Figures/Binary_data_Input.jpg">
<br> 
<br> 
The outputs binary data has confusion matrix(CM), barplot, and lineplot:
<img src="https://github.com/Charlene717/CellCheck/blob/main/Figures/Binary_data.jpg">

## Discrete multiple data
<br> 
The format of input discrete multiple data can be number or character:
<img src="https://github.com/Charlene717/CellCheck/blob/main/Figures/Discrete_multiple_data_Input.jpg">
<br> 
<br> 
The outputs discrete multiple data has confusion matrix(CM), barplot, and lineplot:
<img src="https://github.com/Charlene717/CellCheck/blob/main/Figures/Discrete_multiple_data.jpg">

## Continuous data
<br> 
The format of input continuous data would be number:
<img src="https://github.com/Charlene717/CellCheck/blob/main/Figures/Continuous_data_Input.jpg">
<br> 
<br> 
The outputs continuous data has barplot, and lineplot:
<img src="https://github.com/Charlene717/CellCheck/blob/main/Figures/Continuous_data.jpg">


