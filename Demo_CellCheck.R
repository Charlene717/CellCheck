##### To-do list ######
# - [ ] 3 Main Function
# - [ ] Better simulation demo
# - [ ] Debug: build all metrics
# - [ ] Debug: CTChose1
# - [ ] Combine two dataframe

# - [ ] Multiple lineplot (Group)
# - [ ] BeautifyPlots: Modify color in different group
# - [ ] R, RMSDm mAD
# - [ ] Anova, T-test
#
# - [ ] Goodness of fit index(GFI)
# - [ ] Clean up the code


##### Presetting ######
  rm(list = ls()) # Clean variable
  memory.limit(150000)

##### Load Packages #####
  ## Check whether the installation of those packages is required
  Package.set <- c("tidyverse","caret","cvms","DescTools","devtools","ggthemes") # library(Seurat)
  for (i in 1:length(Package.set)) {
    if (!requireNamespace(Package.set[i], quietly = TRUE)){
      install.packages(Package.set[i])
    }
  }
  ## Load Packages
  lapply(Package.set, library, character.only = TRUE)
  rm(Package.set,i)

  ## install CellCheck
  # Install the CellCheck package
  detach("package:CellCheck", unload = TRUE)
  devtools::install_github("Charlene717/CellCheck")
  ## Load CellCheck
  library(CellCheck)

##### Current path and new folder setting*  #####
  ProjectName = "CC"
  Version = paste0(Sys.Date(),"_",ProjectName,"_Demo")
  Save.Path = paste0(getwd(),"/",Version)
  dir.create(Save.Path)  # Create folder


##### Load simulation datafrme* #####

  # #### Load simulation datafrme by RData  ####
  # load(paste0("./DemoFile/Create_simulation_datafrme3.RData"))

  # load("Create_simulation_datafrme3.RData")
  # write.table(Simu_Anno.df, file=paste0(Save.Path,"/",ProjectName,"_Simu_Anno.tsv"),sep="\t",
  #             row.names=F, quote = FALSE)
  # write.table(Simu_Bi2.df, file=paste0(Save.Path,"/",ProjectName,"_Simu_Bi.tsv"),sep="\t",
  #             row.names=F, quote = FALSE)
  # write.table(Simu_DisMult.df, file=paste0(Save.Path,"/",ProjectName,"_Simu_DisMult.tsv"),sep="\t",
  #             row.names=F, quote = FALSE)
  # write.table(Simu_Conti.df, file=paste0(Save.Path,"/",ProjectName,"_Simu_Conti.tsv"),sep="\t",
  #             row.names=F, quote = FALSE)


  #### Load simulation datafrme by tsv  ####
  Simu_Anno.df <-read.delim(file=paste0("./DemoFile/",ProjectName,"_Simu_Anno.tsv"),sep="\t")
  Simu_Bi.df <-read.delim(file=paste0("./DemoFile/",ProjectName,"_Simu_Bi.tsv"),sep="\t")
  Simu_DisMult.df <-read.delim(file=paste0("./DemoFile/",ProjectName,"_Simu_DisMult.tsv"),sep="\t")
  Simu_Conti.df <-read.delim(file=paste0("./DemoFile/",ProjectName,"_Simu_Conti.tsv"),sep="\t")

#####-----------------------------------(Binary data)-----------------------------------#####
  ## For one prediction
  CMSet.lt <- list(Mode = "One", Actual = "Actual", Predict = "Predict2" , Remark = "_Predict2") # Mode = c("One","Multiple")
  BarChartSet.lt <- list(Mode = "One", Metrics = "Accuracy", XValue = "Type", Group = "Tool", Remark = "_Tool")
  LinePlotSet.lt <- list(Mode = "One", Metrics = "Accuracy", XValue = "PARM", Group = "Tool", Remark = "_Tool")
  CCR_cm_Bi.lt <- CellCheck_Bi(Simu_Bi.df, Simu_Anno.df,
                               CMSet.lt = CMSet.lt,
                               BarChartSet.lt = BarChartSet.lt,
                               LinePlotSet.lt = LinePlotSet.lt,
                               Save.Path = Save.Path, ProjectName = ProjectName)

  ## For multiple prediction
  CMSet.lt <- list(Mode = "Multiple", Remark = "_All") # Mode = c("One","Multiple")
  BarChartSet.lt <- list(Mode = "Multiple", XValue = "Type", Group = "Tool", Remark = "_All")
  LinePlotSet.lt <- list(Mode = "Multiple", XValue = "PARM", Group = "Tool", Remark = "_All")
  CCR_Sum_Bi.df <- CellCheck_Bi(Simu_Bi.df, Simu_Anno.df,
                                CMSet.lt = CMSet.lt,
                                BarChartSet.lt = BarChartSet.lt,
                                LinePlotSet.lt=LinePlotSet.lt,
                                Save.Path = Save.Path, ProjectName = ProjectName)


#####--------------------------(Discrete Multiple data)--------------------------#####
  ## For one prediction
  DisCMSet.lt = list(Mode = "One", Actual = "Actual", Predict = "Predict2" , CTChose1 = "Tool", CTChose2 = "ToolA" , Remark = "") # Mode = c("One","Multiple")
  BarChartSet.lt <- list(Mode = "One", Metrics = "Balanced.Accuracy", XValue = "Type", Group = "Tool", Remark = "")
  LinePlotSet.lt <- list(Mode = "One", Metrics = "Balanced.Accuracy", XValue = "PARM", Group = "Tool", Remark = "")
  cm_DisMult.lt <- CellCheck_DisMult(Simu_DisMult.df, Simu_Anno.df,
                                     DisCMSet.lt = DisCMSet.lt,
                                     BarChartSet.lt = BarChartSet.lt,
                                     LinePlotSet.lt=LinePlotSet.lt,
                                     Save.Path = Save.Path, ProjectName = ProjectName)
  ## For multiple prediction
  DisCMSet.lt = list(Mode = "Multiple", Actual = "Actual", CTChose1 = "Tool", CTChose2 = "ToolA" , Remark = "_All") # Mode = c("One","Multiple")
  BarChartSet.lt <- list(Mode = "Multiple", XValue = "Type", Group = "Tool", Remark = "_All")
  LinePlotSet.lt <- list(Mode = "Multiple", XValue = "PARM", Group = "Tool", Remark = "_All")
  Sum_DisMult.df <- CellCheck_DisMult(Simu_DisMult.df, Simu_Anno.df,
                                      DisCMSet.lt = DisCMSet.lt,
                                      BarChartSet.lt = BarChartSet.lt,
                                      LinePlotSet.lt=LinePlotSet.lt,
                                      Save.Path = Save.Path, ProjectName = ProjectName)


#####---------------------------------(Continuous data)---------------------------------#####
  ## For one index
  BarMetricSet.lt <- list(XValue = "Type", Metrics = "RMSE", Group = "Tool")
  cm_Conti.lt <- CellCheck_Conti(Simu_Bi.df, Simu_Anno.df, Mode = "One", BarMetricSet.lt,
                                 Save.Path = Save.Path, ProjectName = ProjectName)
  ## For multiple index
  Sum_Conti.df <- CellCheck_Conti(Simu_Bi.df, Simu_Anno.df, Mode = "Multiple",BarMetricSet.lt,
                                  Save.Path = Save.Path, ProjectName = ProjectName)


#### Export RData files ####
save.image(paste0(Save.Path,"/",ProjectName,"Demo_CellTypeAnno.RData"))


