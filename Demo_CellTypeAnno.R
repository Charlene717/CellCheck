##### To Do List ######
## Discrete data: Binary data
  # - [ ] Confusion matrix (Simple version)
  #   - [x] Basic version
  #   - [ ] Formula
  #   - [ ] Beautify
  # - [ ] Confusion matrix (Full version)
  #   - [x] Basic version
  #   - [ ] Formula
  #   - [ ] Create CM function by myself
  #   - [ ] Beautify

  # - [x] Annotation table
  # - [x] Compare different conditions
  #   - [ ] Bar
  #   - [ ] Bar with SD
  #   - [ ] Bar with pvalue



## Discrete data: Multiple data
  # - [ ] Confusion matrix
    #   - [x] Basic version
    #   - [ ] Beautify
  # - [ ] Annotation table
  # - [ ] Compare different conditions
  #   - [ ] Bar
  #   - [ ] Bar with SD
  #   - [ ] Bar with pvalue

## Continuous data
  # - [ ] RMSD
# - [ ] Compare different conditions
  #   - [ ] Bar
  #   - [ ] Bar with SD
  #   - [ ] Bar with pvalue

## Beautify Figures
  # - [x] BarChart
  # - [ ] LineChart
  # - [ ] Various templates
  # - [ ] TIFF, PNG
  # - [ ] Function for adjusting detail
  # - [ ] Load package



##### Presetting ######
  rm(list = ls()) # Clean variable
  memory.limit(150000)

##### Load Packages #####
  library(Seurat)
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

  Version = paste0(Sys.Date(),"_","CC_Demo")
  Save.Path = paste0(getwd(),"/",Version)
  dir.create(Save.Path)


##### Load simulation datafrme #####
  load("D:/Dropbox/##_GitHub/##_CAESAR/CellCheck/Create_simulation_datafrme.RData")


##### Binary data #####
  #### calculate the confusion matrix ####
    library(caret)

    cm.lt <- list()

    for (i in 1:(ncol(Simu_Bi.df)-1)) {
      cm.lt[[i]] <- confusionMatrix(data = Simu_Bi.df[,1] %>% as.factor(),
                            reference = Simu_Bi.df[,1+i] %>% as.factor())
      names(cm.lt)[[i]] <- colnames(Simu_Bi.df)[i+1]

    }
    rm(i)

    # cm <- confusionMatrix(data = Simu_Bi.df$Actual %>% as.factor(),
    #                       reference = Simu_Bi.df$Predict1 %>% as.factor())

  ####  Plot Result by Bar chart of Metrics ####
    ## Result dataframe
    Results_Bi.df <- Summarize_BiCM(cm.lt, Anno.df)

    ## Plot by group
    p1 <- CC_BarPlot(Results_Bi.df, MetricsX = "Type", MetricsY = "Accuracy", MetricsG = "Tool")
    p2 <- CC_BarPlot(Results_Bi.df, MetricsX = "Type",Metrics = "Kappa", MetricsG = "Tool")
    rm(p1,p2)

    Metrics_Bi.set <- colnames(Results_Bi.df)[2:(ncol(Results_Bi.df)-(ncol(Anno.df)-1))]

    #### Export BarPlot PDF ####
    pdf(file = paste0(Save.Path,"/",ProjectName,"_Bi_MetricsBar.pdf"),
      width = 7,  height = 7)
      for (i in 1:length(Metrics_Bi.set)) {
          p <- CC_BarPlot(Results_Bi.df, Metrics = Metrics_Bi.set[i])
          p
      }
    # dev.off()
    graphics.off()
    rm(p,i)

  ##### Plot Confusion matrix #####
      ## Extract the confusion matrix
        cm <- cm.lt[["Predict2"]]

      ## Draw Confusion matrix
        source("Fun_Draw_ConfuMax.R")
        draw_confusion_matrix(cm)
        Draw_CM(cm)

        ## Full version
          pdf(
            file = paste0(Save.Path,"/",ProjectName,"_Bi_ConfuMax.pdf"),
            width = 17,  height = 12
          )
            for (i in 1:length(cm.lt)) {

              Draw_CM(cm.lt[[i]],names(cm.lt[i]))
            }
          dev.off()
          #graphics.off()
          rm(i)

        ## Simple version
          pdf(
            file = paste0(Save.Path,"/",ProjectName,"_Bi_ConfuMaxSimp.pdf"),
            width = 10,  height = 7
          )
          for (i in 1:length(cm.lt)) {

            draw_confusion_matrix(cm.lt[[i]],names(cm.lt[i]))
          }
          dev.off()
          #graphics.off()
          rm(i)


#########################################################################################################
  ##### Misclassification rate (error rate) #####
    # Simu_DisMult.df$Correctness1 <- ""

    Simu_DisMult_Res.df <- data.frame(matrix("",0,(ncol(Simu_DisMult.df)-1)))
    colnames(Simu_DisMult_Res.df) <- colnames(Simu_DisMult.df)[2:ncol(Simu_DisMult.df)]

    # MissRate: Misclassification rate
      cm_DisMult.lt <- list()
      for (j in 1:(ncol(Simu_DisMult.df)-1)) {

        for (i in 1:nrow(Simu_DisMult.df)) {
          if(Simu_DisMult.df[i,1] == Simu_DisMult.df[i,j+1]){
            Simu_DisMult_Res.df[i,j] = 0
          }else{
            Simu_DisMult_Res.df[i,j] = 1
          }
        }
        cm_DisMult.lt[j] <- sum(Simu_DisMult_Res.df[,j] == 1)/nrow(Simu_DisMult_Res.df)
        names(cm_DisMult.lt)[j] <- colnames(Simu_DisMult_Res.df)[j]
      }

      rm(i,j,Simu_DisMult_Res.df)
      Results_DisMult.df <- data.frame(TestID = colnames(Simu_DisMult.df)[2:ncol(Simu_DisMult.df)],
                                       Misclass = unlist(cm_DisMult.lt),
                                       Accuracy = 1-unlist(cm_DisMult.lt))

    ####  Plot Result by Bar chart of Metrics ####
      ## Result dataframe
      Results_DisMult.df <- left_join(Results_DisMult.df, Anno.df)

      ## Plot by group
      p1 <- CC_BarPlot(Results_DisMult.df, MetricsX = "Type", MetricsY = "Accuracy", MetricsG = "Tool")
      p1
      rm(p1)

      Metrics_DisMult.set <- colnames(Results_DisMult.df)[2:(ncol(Results_DisMult.df)-(ncol(Anno.df)-1))]

      #### Export BarPlot PDF ####
      pdf(file = paste0(Save.Path,"/",ProjectName,"_DisMult__MetricsBar.pdf"),
          width = 7,  height = 7)
      for (i in 1:length(Metrics_DisMult.set)) {
        p <- CC_BarPlot(Results_DisMult.df, MetricsX = "Type",
                        MetricsY = Metrics_DisMult.set[i], MetricsG = "Tool")
        p
      }
      dev.off()
      #graphics.off()
      rm(p,i)

    ##### calculate the confusion matrix for Multi-Class Classification #####
    ## Ref: https://www.researchgate.net/figure/Confusion-matrix-for-60-training-and-40-testing-strategy_fig4_338909223
    ## Ref: https://cran.r-project.org/web/packages/cvms/vignettes/Creating_a_confusion_matrix.html
      # install.packages("cvms")
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
      # install.packages("DescTools")
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
      MA.df <- left_join(MA.df, Anno.df)
      MA.set <- colnames(MA.df)[2:(ncol(MA.df)-ncol(Anno.df)+1)]

      #### Export BarPlot PDF ####
      pdf(file = paste0(Save.Path,"/",ProjectName,"_Conti_MetricsBar.pdf"),
          width = 7,  height = 7)
        for (i in 1:length(MA.set)) {
          p <- CC_BarPlot(MA.df, Metrics = MA.set[i])
          p
        }
      # dev.off()
      graphics.off()
      rm(p,i)


      #### Export LinePlot PDF ####
      MA.df$PARM <- factor(MA.df$PARM,levels = sort(seq(1:15), decreasing = TRUE))
      p <- CC_LinePlot(MA.df, MetricsX = "PARM", MetricsY = "RMSE", MetricsG = "Type")


      #### Export BarPlot PDF ####
      pdf(file = paste0(Save.Path,"/",ProjectName,"_Conti_MetricsLine.pdf"),
          width = 12,  height = 7)
        for (i in 1:length(MA.set)) {
          p <- CC_LinePlot(MA.df, MetricsX = "PARM", MetricsY = MA.set[i], MetricsG = "Type")
          p
        }
      # dev.off()
      graphics.off()
      rm(p,i)




