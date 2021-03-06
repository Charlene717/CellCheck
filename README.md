# CellCheck <img src="Figures/CellCheck.png" align="right" width="120" />
## A toolkit for validating single-cell analysis with visualization

## Overview
CellCheck is a toolkit for validating single-cell analysis with visualization.
<br> User can use CellCheck to verify malignancy annotation, cell type annotation, scoring, and deconvolution results from the different conditions. For example, user can compare the performance of different scRNA-seq analysis tools, different improved algorithms, and optimize the parameters used in these tools.
<br> 
<br> It can also be applied to other use. For example, it can be extended to find biomarkers or gene signatures from databases such as TCGA or other databases.

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
  Package.set <- c("tidyverse","caret","cvms","DescTools","devtools","ggthemes")
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
source("Demo_CellCheck.R", echo = TRUE, max.deparse.length=10000, encoding="utf-8",
       print.eval = TRUE) 
```

#### Current path and new folder setting ####
```{r, eval = FALSE}
  ProjectName = "CC"
  Version = paste0(Sys.Date(),"_",ProjectName,"_Demo")
  Save.Path = paste0(getwd(),"/",Version)
  dir.create(Save.Path)  # Create folder
```
  
## Input and export files
### Binary data
The format of input binary data can be number or character:
<img src="https://github.com/Charlene717/CellCheck/blob/main/Figures/Binary_data_Input.jpg">
<br>

Run the code:
```{r, eval = FALSE}
  ## For one prediction
  CMSet.lt = list(Mode = "One", Actual = "Actual", Predict = "Predict2" , Remark = "_Predict2") # Mode = c("One","Multiple")
  BarChartSet.lt = list(Mode = "One",  Metrics = "Accuracy", XValue = "Type", Group = "Tool", Remark = "_Tool")
  LinePlotSet.lt = list(Mode = "One", Metrics = "Accuracy", XValue = "PARM", Group = "Tool", Remark = "_Tool")
  CCR_cm_Bi.lt <- CellCheck_Bi(Simu_Bi.df, Simu_Anno.df,
                               CMSet.lt = CMSet.lt,
                               BarChartSet.lt = BarChartSet.lt,
                               LinePlotSet.lt = LinePlotSet.lt,
                               Save.Path = Save.Path, ProjectName = ProjectName)

  ## For multiple prediction
  CMSet.lt = list(Mode = "Multiple", Remark = "_All") # Mode = c("One","Multiple")
  BarChartSet.lt = list(Mode = "Multiple", XValue = "Type", Group = "Tool", Remark = "_All")
  LinePlotSet.lt = list(Mode = "Multiple", XValue = "PARM", Group = "Tool", Remark = "_All")
  CCR_Sum_Bi.df <- CellCheck_Bi(Simu_Bi.df, Simu_Anno.df,
                                CMSet.lt = CMSet.lt,
                                BarChartSet.lt = BarChartSet.lt,
                                LinePlotSet.lt=LinePlotSet.lt,
                                Save.Path = Save.Path, ProjectName = ProjectName)
```

<br> 
The outputs binary data has confusion matrix(CM), barplot, and lineplot:
<img src="https://github.com/Charlene717/CellCheck/blob/main/Figures/Binary_data.jpg">
<br> 


## Discrete multiple data
The format of input discrete multiple data can be number or character:
<img src="https://github.com/Charlene717/CellCheck/blob/main/Figures/Discrete_multiple_data_Input.jpg">
<br>

Run the code:
```{r, eval = FALSE}

  ## For one prediction
  DisCMSet.lt = list(Mode = "One", Actual = "Actual", Predict = "Predict2" , FilterSet1 = "Tool", FilterSet2 = "ToolA" , Remark = "") # Mode = c("One","Multiple")
  BarChartSet.lt <- list(Mode = "One", Metrics = "Balanced.Accuracy", XValue = "Type", Group = "Tool", Remark = "")
  LinePlotSet.lt <- list(Mode = "One", Metrics = "Balanced.Accuracy", XValue = "PARM", Group = "Tool", Remark = "")
  cm_DisMult.lt <- CellCheck_DisMult(Simu_DisMult.df, Simu_Anno.df,
                                     DisCMSet.lt = DisCMSet.lt,
                                     BarChartSet.lt = BarChartSet.lt,
                                     LinePlotSet.lt=LinePlotSet.lt,
                                     Save.Path = Save.Path, ProjectName = ProjectName)
  ## For multiple prediction
  DisCMSet.lt = list(Mode = "Multiple", Actual = "Actual", FilterSet1 = "Tool", FilterSet2 = "ToolA" , Remark = "_All") # Mode = c("One","Multiple")
  BarChartSet.lt <- list(Mode = "Multiple", XValue = "Type", Group = "Tool", Remark = "_All")
  LinePlotSet.lt <- list(Mode = "Multiple", XValue = "PARM", Group = "Tool", Remark = "_All")
  Sum_DisMult.df <- CellCheck_DisMult(Simu_DisMult.df, Simu_Anno.df,
                                      DisCMSet.lt = DisCMSet.lt,
                                      BarChartSet.lt = BarChartSet.lt,
                                      LinePlotSet.lt=LinePlotSet.lt,
                                      Save.Path = Save.Path, ProjectName = ProjectName)


```

<br> 
The outputs discrete multiple data has confusion matrix(CM), barplot, and lineplot:
<img src="https://github.com/Charlene717/CellCheck/blob/main/Figures/Discrete_multiple_data.jpg">
<br> 

## Continuous data
The format of input continuous data would be number:
<img src="https://github.com/Charlene717/CellCheck/blob/main/Figures/Continuous_data_Input.jpg">
<br> 

Run the code:
```{r, eval = FALSE}
  ## For one index
  BarChartSet.lt = list(Mode = "One", Metrics = "RMSE", XValue = "Type", Group = "Tool", Remark = "") # Mode = c("One","Multiple")
  LinePlotSet.lt = list(Mode = "One", Metrics = "RMSE", XValue = "PARM", Group = "Tool", Remark = "")
  cm_Conti.lt <- CellCheck_Conti(Simu_Bi.df, Simu_Anno.df,
                                 BarChartSet.lt = BarChartSet.lt,
                                 LinePlotSet.lt =  LinePlotSet.lt,
                                 Save.Path = Save.Path, ProjectName = ProjectName)
  ## For multiple index
  BarChartSet.lt <- list(Mode = "Multiple", XValue = "Type", Group = "Tool", Remark = "_All")
  LinePlotSet.lt <- list(Mode = "Multiple", XValue = "PARM", Group = "Tool", Remark = "_All")
  Sum_Conti.df <- CellCheck_Conti(Simu_Bi.df, Simu_Anno.df,
                                  BarChartSet.lt = BarChartSet.lt,
                                  LinePlotSet.lt =  LinePlotSet.lt,
                                  Save.Path = Save.Path, ProjectName = ProjectName)

```

<br> 
The outputs continuous data has barplot, and lineplot:
<img src="https://github.com/Charlene717/CellCheck/blob/main/Figures/Continuous_data.jpg">
<br> 

