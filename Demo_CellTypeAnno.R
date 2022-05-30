
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
  #####
  # load("Create_simulation_datafrme3.RData")
  # write.table(Simu_Anno.df, file=paste0(Save.Path,"/",ProjectName,"_Simu_Anno.tsv"),sep="\t",
  #             row.names=F, quote = FALSE)
  # write.table(Simu_Bi2.df, file=paste0(Save.Path,"/",ProjectName,"_Simu_Bi.tsv"),sep="\t",
  #             row.names=F, quote = FALSE)
  # write.table(Simu_DisMult.df, file=paste0(Save.Path,"/",ProjectName,"_Simu_DisMult.tsv"),sep="\t",
  #             row.names=F, quote = FALSE)
  # write.table(Simu_Conti.df, file=paste0(Save.Path,"/",ProjectName,"_Simu_Conti.tsv"),sep="\t",
  #             row.names=F, quote = FALSE)
  #####
  Simu_Anno.df <-read.delim(file=paste0("./#_Input_Simu/",ProjectName,"_Simu_Anno.tsv"),sep="\t")
  Simu_Bi.df <-read.delim(file=paste0("./#_Input_Simu/",ProjectName,"_Simu_Bi.tsv"),sep="\t")
  Simu_DisMult.df <-read.delim(file=paste0("./#_Input_Simu/",ProjectName,"_Simu_DisMult.tsv"),sep="\t")
  Simu_Conti.df <-read.delim(file=paste0("./#_Input_Simu/",ProjectName,"_Simu_Conti.tsv"),sep="\t")

#####-----------------------------------(Binary data)-----------------------------------#####
#### Calculate the confusion matrix(CM) ####
  CMPredSet.lt <- list(Actual = "Actual", Predict = "Predict2")
  cm_Bi.lt <- CellCheck_Bi(Simu_Bi.df, Simu_Anno.df, Mode = "One", CMPredSet.lt,
                            Save.Path = Save.Path, ProjectName = ProjectName)
  Sum_Bi.df <- CellCheck_Bi(Simu_Bi.df, Simu_Anno.df, Mode = "Multiple", CMPredSet.lt,
                            Save.Path = Save.Path, ProjectName = ProjectName)


#####--------------------------(Discrete Multiple data)--------------------------#####

##### calculate the confusion matrix for Multi-Class Classification #####
  DisMultCM.set <- "Predict2"
  cm_DisMult.lt <- CellCheck_DisMult(Simu_DisMult, Simu_Anno.df, Mode = "One", DisMultCM.set,
                                   Save.Path = Save.Path, ProjectName = ProjectName)
  Sum_DisMult.df <- CellCheck_DisMult(Simu_DisMult, Simu_Anno.df, Mode = "Multiple", DisMultCM.set,
                                      Save.Path = Save.Path, ProjectName = ProjectName)


#####---------------------------------(Continuous data)---------------------------------#####

  library(DescTools)

  # Sum_Conti.df <- Conti_Accuracy(Simu_Conti.df$Actual,Simu_Conti.df$Predict2)

  #### Create Measure accuracy dataframe (Sum_Conti.df) ####
  for (i in 1:(ncol(Simu_Conti.df)-1)) {
    if(i==1){
      Sum_Conti.df <- Conti_Accuracy(Simu_Conti.df[,1],
                              Simu_Conti.df[,1+i])
      row.names(Sum_Conti.df) <- colnames(Simu_Conti.df)[i+1]
    }else{
      MA_New.df <- Conti_Accuracy(Simu_Conti.df[,1],
                                  Simu_Conti.df[,1+i])
      row.names(MA_New.df) <- colnames(Simu_Conti.df)[i+1]
      Sum_Conti.df <- rbind(Sum_Conti.df, MA_New.df)
    }
  }
  rm(i,MA_New.df)

  Sum_Conti.df <- data.frame(TestID = row.names(Sum_Conti.df), Sum_Conti.df)
  Sum_Conti.df <- left_join(Sum_Conti.df, Simu_Anno.df)

  #### Export MetricBar PDF ####
    #### Export one Designated MetricBar PDF ####
    ## Plot by Designated Metric
    BarMetricSet.lt <- list(XValue = "Type", Metrics = "RMSE", Group = "Tool")
    p1 <- CC_BarPlot(Sum_Conti.df,
                     XValue = BarMetricSet.lt[["XValue"]],
                     Metrics = BarMetricSet.lt[["Metrics"]],
                     Group = BarMetricSet.lt[["Group"]])
    ## Export MetricBar PDF
    pdf(file = paste0(Save.Path,"/",ProjectName,"_Conti_MetricsBar_",BarMetricSet.lt[["Metrics"]],".pdf"),
        width = 7,  height = 7
    )
      p1
    dev.off()
    rm(p1)

    #### Export all MetricBar PDF ####
    MA.set <- colnames(Sum_Conti.df)[2:(ncol(Sum_Conti.df)-ncol(Simu_Anno.df)+1)]
    pdf(file = paste0(Save.Path,"/",ProjectName,"_Conti_MetricsBar.pdf"),
        width = 7,  height = 7)
      for (i in 1:length(MA.set)) {
        p <- CC_BarPlot(Sum_Conti.df,
                        XValue = BarMetricSet.lt[["XValue"]],
                        Metrics = MA.set[i],
                        Group = BarMetricSet.lt[["Group"]])
        p
      }
    dev.off()  #graphics.off()
    rm(p,i,BarMetricSet.lt,MA.set)


  #### Export MetricsLine PDF ####
  Sum_Conti.df$PARM <- factor(Sum_Conti.df$PARM,levels = sort(seq(1:15), decreasing = TRUE))

    #### Export one Designated MetricLine PDF ####
    ## Plot by Designated Metrics
    BarMetricSet.lt <- list(XValue = "PARM", Metrics = "RMSE", Group = "Tool")
    p <- CC_LinePlot(Sum_Conti.df,
                     XValue = BarMetricSet.lt[["XValue"]],
                     Metrics = BarMetricSet.lt[["Metrics"]],
                     Group = BarMetricSet.lt[["Group"]])
    rm(p)

    #### Export all MetricLine PDF ####
    MA.set <- colnames(Sum_Conti.df)[2:(ncol(Sum_Conti.df)-ncol(Simu_Anno.df)+1)]

    pdf(file = paste0(Save.Path,"/",ProjectName,"_Conti_MetricsLine.pdf"),
        width = 12,  height = 7)
      for (i in 1:length(MA.set)) {
        p <- CC_LinePlot(Sum_Conti.df,
                         XValue = BarMetricSet.lt[["XValue"]],
                         Metrics = MA.set[i],
                         Group = BarMetricSet.lt[["Group"]])
        p
      }
    dev.off() #graphics.off()
    rm(p,i,BarMetricSet.lt)

  #### Export tsv files ####
  write.table(Sum_Conti.df, file=paste0(Save.Path,"/",ProjectName,"_Sum_Conti.tsv"),sep="\t",
              row.names=F, quote = FALSE)

#### Export RData files ####
save.image(paste0(Save.Path,"/",ProjectName,"Demo_CellTypeAnno.RData"))


