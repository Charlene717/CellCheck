
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
  CMPredSet.lt <- list(Actual = "Actual", Predict = "Predict2")
  cm_Bi.lt <- CellCheck_Bi(Simu_Bi.df, Simu_Anno.df, Mode = "One", CMPredSet.lt,
                            Save.Path = Save.Path, ProjectName = ProjectName)
  ## For multiple prediction
  Sum_Bi.df <- CellCheck_Bi(Simu_Bi.df, Simu_Anno.df, Mode = "Multiple",
                            Save.Path = Save.Path, ProjectName = ProjectName)


#####--------------------------(Discrete Multiple data)--------------------------#####
  ## For one prediction
  DisMultCM.lt <- list(Actual = "Actual", Predict = "Predict2")
  cm_DisMult.lt <- CellCheck_DisMult(Simu_DisMult.df, Simu_Anno.df, Mode = "One", DisMultCM.lt,
                                   Save.Path = Save.Path, ProjectName = ProjectName)
  ## For multiple prediction
  Sum_DisMult.df <- CellCheck_DisMult(Simu_DisMult.df, Simu_Anno.df, Mode = "Multiple",
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


