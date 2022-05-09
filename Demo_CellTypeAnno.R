
##### Presetting ######
  rm(list = ls()) # Clean variable
  memory.limit(150000)

##### Load Packages #####
  ## Check whether the installation of the package is required
  Package.lt <- c("tidyverse","caret","cvms","DescTools")
  for (i in 1:length(Package.lt)) {
    if (!requireNamespace(Package.lt[i], quietly = TRUE)){
      install.packages(Package.lt[i])
    }
  }
  rm(Package.lt,i)

  ## Load Packages
  # library(Seurat)
  library(tidyverse)
  library(caret) # Confusion matrix
  library(cvms) # Confusion matrix for Multi-Class Classification
  library(DescTools) # Measures of Accuracy

##### Function setting #####
  ## Call function
  source("Fun_Draw_ConfuMax.R")
  source("FUN_Draw_ConfuMatrix.R")
  source("FUN_Summarize_BiCM.R")
  source("FUN_CC_BarPlot.R")
  source("FUN_CC_LinePlot.R")
  source("FUN_Measure_Accuracy.R")

##### Current path and new folder setting*  #####
  ProjectName = "CC"
  Version = paste0(Sys.Date(),"_",ProjectName,"_Demo")
  Save.Path = paste0(getwd(),"/",Version)
  dir.create(Save.Path)  # Create folder


##### Load simulation datafrme #####
  load("Create_simulation_datafrme.RData")

#####------------------------------------------------------------------------------------#####
##### Binary data #####
  #### Calculate the confusion matrix(CM) ####
    library(caret)

    #### For one prediction ####
    ## Set two comparisons
    CMPredSet.lt <- list(Actual = "Actual",
                          Predict = "Predict2")

    ## Build CM
    cm_Bi.df <- confusionMatrix(data = Simu_Bi.df[,CMPredSet.lt[["Actual"]]] %>% as.factor(),
                             reference = Simu_Bi.df[,CMPredSet.lt[["Predict"]]] %>% as.factor())

    ## Plot CM
    p1 <- Draw_CM(cm_Bi.df)
    p2 <- draw_confusion_matrix(cm_Bi.df)

    ## Export CM PDF
    pdf(file = paste0(Save.Path,"/",ProjectName,"_Bi_ConfuMax_",CMPredSet.lt[["Predict"]],".pdf"),
        width = 17,  height = 12
    )
      Draw_CM(cm_Bi.df)
      draw_confusion_matrix(cm_Bi.df)
    dev.off()
    rm(p1,p2,CMPredSet.lt,cm_Bi.df)

    #### For all predictions ####
    ## Build list for all CM
    cm_Bi.lt <- list()
    for (i in 1:(ncol(Simu_Bi.df)-1)) {
      cm_Bi.lt[[i]] <- confusionMatrix(data = Simu_Bi.df[,1] %>% as.factor(),
                                       reference = Simu_Bi.df[,1+i] %>% as.factor())
      names(cm_Bi.lt)[[i]] <- colnames(Simu_Bi.df)[i+1]
    }
    rm(i)

    ## Build summarize dataframe
    Sum_Bi.df <- Summarize_BiCM(cm_Bi.lt, Simu_Anno.df)

      # ## Extract one CM form the CM list
      # cm_Bi.df <- cm_Bi.lt[["Predict2"]]
      #
      # ## Draw Confusion matrix
      # source("Fun_Draw_ConfuMax.R")
      # draw_confusion_matrix(cm_Bi.df)
      # Draw_CM(cm_Bi.df)
      # rm(cm_Bi.df)

    #### Export all CM PDF ####
      ## Full version
      pdf(
        file = paste0(Save.Path,"/",ProjectName,"_Bi_ConfuMax.pdf"),
        width = 17,  height = 12
      )
      for (i in 1:length(cm_Bi.lt)) {

        Draw_CM(cm_Bi.lt[[i]],names(cm_Bi.lt[i]))
      }
      dev.off() #graphics.off()
      rm(i)

      ## Simple version
      pdf(
        file = paste0(Save.Path,"/",ProjectName,"_Bi_ConfuMaxSimp.pdf"),
        width = 10,  height = 7
      )
      for (i in 1:length(cm_Bi.lt)) {

        draw_confusion_matrix(cm_Bi.lt[[i]],names(cm_Bi.lt[i]))
      }
      dev.off() #graphics.off()
      rm(i)


    #### Export MetricBar PDF ####
      #### Export Designated one MetricBar PDF ####
      ## Plot by Designated Metric
      BarMetricSet.lt <- list(XValue = "Type", Metrics = "Accuracy", Group = "Tool")

      p1 <- CC_BarPlot(Sum_Bi.df,
                       XValue = BarMetricSet.lt[["XValue"]],
                       Metrics = BarMetricSet.lt[["Metrics"]],
                       Group = BarMetricSet.lt[["Group"]])

      ## Export MetricBar PDF
      pdf(file = paste0(Save.Path,"/",ProjectName,"_Bi_MetricsBar_",BarMetricSet.lt[["Metrics"]],".pdf"),
          width = 7,  height = 7
      )
      p1
      dev.off()
      rm(p1)

      #### Export all MetricBar PDF ####
      Metrics_Bi.set <- colnames(Sum_Bi.df)[2:(ncol(Sum_Bi.df)-(ncol(Simu_Anno.df)-1))]

      pdf(file = paste0(Save.Path,"/",ProjectName,"_Bi_MetricsBar.pdf"),
          width = 7,  height = 7)
        for (i in 1:length(Metrics_Bi.set)) {
          p <- CC_BarPlot(Sum_Bi.df,
                          XValue = BarMetricSet.lt[["XValue"]],
                          Metrics = Metrics_Bi.set[i],
                          Group = BarMetricSet.lt[["Group"]])
          p
        }
      dev.off() # graphics.off()
      rm(p, i, Metrics_Bi.set, BarMetricSet.lt)


    #### Export MetricsLine PDF ####
      Sum_Bi.df$PARM <- factor(Sum_Bi.df$PARM,levels = sort(seq(1:15), decreasing = TRUE))

      LineMetricSet.lt <- list(XValue = "PARM", Metrics = "Accuracy", Group = "Type")
      p <- CC_LinePlot(Sum_Bi.df, XValue = LineMetricSet.lt[["XValue"]],
                       Metrics = LineMetricSet.lt[["Metrics"]],
                       Group = LineMetricSet.lt[["Group"]])
      #### Export one Designated MetricLine PDF ####
      pdf(file = paste0(Save.Path,"/",ProjectName,"_Bi_MetricsLine_",LineMetricSet.lt[["Metrics"]],".pdf"),
          width = 12,  height = 7
      )
        p
      dev.off()
      rm(p)

      #### Export all MetricLine PDF ####
      Sum_Bi.set <- colnames(Sum_Bi.df)[2:(ncol(Sum_Bi.df)-ncol(Simu_Anno.df)+1)]
      pdf(file = paste0(Save.Path,"/",ProjectName,"_Bi_MetricsLine.pdf"),
          width = 12,  height = 7)
      for (i in 1:length(Sum_Bi.set)) {
        p <- CC_LinePlot(Sum_Bi.df, XValue = LineMetricSet.lt[["XValue"]],
                         Metrics = Sum_Bi.set[i],
                         Group = LineMetricSet.lt[["Group"]])
        p
      }
      dev.off() #graphics.off()
      rm(p, i, LineMetricSet.lt, Sum_Bi.set)


#####------------------------------------------------------------------------------------#####
##### Discrete data: Multiple data #####
  ##### Calculate Accuracy(ACC) and Misclassification rate (Error Rate, ER) #####
    source("FUN_ACC_ER_DiscMult.R")
    Sum_DisMult.df <- AccEr_DiscMult(Simu_DisMult.df, Simu_Anno.df)

    ####  Plot Result by Bar chart of Metrics ####


      ## Plot by group
      p1 <- CC_BarPlot(Results_DisMult.df, XValue = "Type", Metrics = "Accuracy", Group = "Tool")
      p1
      rm(p1)

      Metrics_DisMult.set <- colnames(Results_DisMult.df)[2:(ncol(Results_DisMult.df)-(ncol(Simu_Anno.df)-1))]

      #### Export BarPlot PDF ####
      pdf(file = paste0(Save.Path,"/",ProjectName,"_DisMult_MetricsBar.pdf"),
          width = 7,  height = 7)
      for (i in 1:length(Metrics_DisMult.set)) {
        p <- CC_BarPlot(Results_DisMult.df, XValue = "Type",
                        Metrics = Metrics_DisMult.set[i], Group = "Tool")
        p
      }
      dev.off()
      #graphics.off()
      rm(p,i)

    ##### calculate the confusion matrix for Multi-Class Classification #####
    ## Ref: https://www.researchgate.net/figure/Confusion-matrix-for-60-training-and-40-testing-strategy_fig4_338909223
    ## Ref: https://cran.r-project.org/web/packages/cvms/vignettes/Creating_a_confusion_matrix.html
      library(cvms)

      ##
      conf_mat <- confusion_matrix(targets = Simu_DisMult.df$Actual,
                                   predictions = Simu_DisMult.df$Predict1)

      conf_mat

      p1 <- plot_confusion_matrix(conf_mat$`Confusion Matrix`[[1]],
                            add_sums = TRUE)

      p1 + theme(axis.text.x = element_text(face="bold", color="#3d3d3d", size=12), #plot.margin = unit(c(2,3,3,4),"cm")
                 axis.text.y = element_text(face="bold", color="#3d3d3d", size=12,angle=0),
                 axis.title.x = element_text(face="bold", color="#3d3d3d", size=18),
                 axis.title.y = element_text(face="bold", color="#3d3d3d", size=18))

      ##
      conf_mat2 <- confusion_matrix(targets = Simu_DisMult.df$Actual,
                                   predictions = Simu_DisMult.df[,3])

      conf_mat2

      plot_confusion_matrix(conf_mat2$`Confusion Matrix`[[1]],
                            add_sums = TRUE)


      #### calculate the confusion matrix ####
      conf_mat.lt <- list()

      for (i in 1:(ncol(Simu_DisMult.df)-1)) {
        conf_mat.lt[[i]] <- confusion_matrix(targets = Simu_DisMult.df[,1] %>% as.factor(),
                                             predictions = Simu_DisMult.df[,1+i] %>% as.factor())
        names(conf_mat.lt)[[i]] <- colnames(Simu_DisMult.df)[i+1]

      }
      rm(i)

      #### Export PDF ####
      pdf(
        file = paste0(Save.Path,"/",ProjectName,"_DisMult_ConfuMax.pdf"),
        width = 10,  height = 10
      )
        for (i in 1:length(conf_mat.lt)) {
          p1 <- plot_confusion_matrix(conf_mat.lt[[i]][["Confusion Matrix"]][[1]],
                                      add_sums = TRUE)

          p1 + ggtitle(names(conf_mat.lt)[i])+
               theme(axis.text.x = element_text(face="bold", color="#3d3d3d", size=12), #plot.margin = unit(c(2,3,3,4),"cm")
                     axis.text.y = element_text(face="bold", color="#3d3d3d", size=12,angle=0),
                     axis.title.x = element_text(face="bold", color="#3d3d3d", size=18),
                     axis.title.y = element_text(face="bold", color="#3d3d3d", size=18),
                     title = element_text(face="bold", color="#3d3d3d", size=18)) -> p2
          print(p2)
        }

      dev.off()
      rm(p1,p2)


#########################################################################################################

      library(DescTools)
      source("FUN_Measure_Accuracy.R")

      # MA.df <- Measure_Accuracy(Simu_Conti.df$Actual,Simu_Conti.df$Predict2)

      #### Create Measure accuracy dataframe (MA.df)
      for (i in 1:(ncol(Simu_Conti.df)-1)) {
        if(i==1){
          MA.df <- Measure_Accuracy(Simu_Conti.df[,1],
                                    Simu_Conti.df[,1+i])
          row.names(MA.df) <- colnames(Simu_Conti.df)[i+1]
        }else{
          MA_New.df <- Measure_Accuracy(Simu_Conti.df[,1],
                                    Simu_Conti.df[,1+i])
          row.names(MA_New.df) <- colnames(Simu_Conti.df)[i+1]
          MA.df <- rbind(MA.df, MA_New.df)

        }

      }
      rm(i,MA_New.df)

      MA.df <- data.frame(TestID=row.names(MA.df), MA.df)
      MA.df <- left_join(MA.df, Simu_Anno.df)
      MA.set <- colnames(MA.df)[2:(ncol(MA.df)-ncol(Simu_Anno.df)+1)]

      #### Export BarPlot PDF ####
      pdf(file = paste0(Save.Path,"/",ProjectName,"_Conti_MetricsBar.pdf"),
          width = 7,  height = 7)
        for (i in 1:length(MA.set)) {
          p <- CC_BarPlot(MA.df, XValue = "Type", Metrics = MA.set[i], Group = "Tool")
          p
        }
      # dev.off()
      graphics.off()
      rm(p,i)


      #### Export LinePlot PDF ####
      MA.df$PARM <- factor(MA.df$PARM,levels = sort(seq(1:15), decreasing = TRUE))
      p <- CC_LinePlot(MA.df, XValue = "PARM", Metrics = "RMSE", Group = "Type")

      pdf(file = paste0(Save.Path,"/",ProjectName,"_Conti_MetricsLine.pdf"),
          width = 12,  height = 7)
        for (i in 1:length(MA.set)) {
          p <- CC_LinePlot(MA.df, XValue = "PARM", Metrics = MA.set[i], Group = "Type")
          p
        }
      dev.off() #graphics.off()
      rm(p,i)




