##### To Do List ######
## Discrete data: Binary data
  # - [ ] Confusion matrix (Simple version)
  #   - [x] Basic version
  #   - [ ] Formula
  #   - [ ] Beautify
  # - [ ] Confusion matrix (Full version)
  #   - [x] Basic version
  #   - [ ] Formula
  #   - [ ] Beautify

  # - [x] Annotation table
  # - [x] Compare different conditions

## Discrete data: Multiple data
  # - [ ] Confusion matrix
  # - [ ] Annotation table
  # - [ ] Compare different conditions

## Continuous data
  # - [ ] RMSD

## Beautify Figures
  # - [ ] Various templates


##### Presetting ######
  rm(list = ls()) # Clean variable
  memory.limit(150000)

##### Load Packages #####
  library(Seurat)
  library(tidyverse)
  library(caret) # Confusion matrix

##### Function setting #####
  ## Call function
  source("Fun_Draw_ConfuMax.R")
  source("FUN_Draw_ConfuMatrix.R")
  source("FUN_Summary_CM.R")
  source("FUN_Plot_CMBar.R")

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

    for (i in 1:(ncol(Check_Bi.df)-1)) {
      cm.lt[[i]] <- confusionMatrix(data = Check_Bi.df[,1] %>% as.factor(),
                            reference = Check_Bi.df[,1+i] %>% as.factor())
      names(cm.lt)[[i]] <- colnames(Check_Bi.df)[i+1]

    }
    rm(i)

    # cm <- confusionMatrix(data = Check_Bi.df$Actual %>% as.factor(),
    #                       reference = Check_Bi.df$Predict1 %>% as.factor())

  ####  Plot Result by Bar chart of Metrics ####
    ## Result dataframe
    Results.df <- Summary_CM(cm.lt, Anno.df)

    # Ref(Bar Chart): https://officeguide.cc/r-ggplot2-bar-plot-tutorial-examples/
    # Ref(Color): http://rstudio-pubs-static.s3.amazonaws.com/5312_98fc1aba2d5740dd849a5ab797cc2c8d.html

    ## Plot by group
    p <- Plot_CMBar(Results.df, Metrics = "Kappa")
    p

    Metrics.set <- colnames(Results.df)[2:(ncol(Results.df)-(ncol(Anno.df)-1))]

    pdf(file = paste0(Save.Path,"/",ProjectName,"_MetricsBar.pdf"),
      width = 7,  height = 7)
      for (i in 1:length(Metrics.set)) {
          p <- Plot_CMBar(Results.df, Metrics = Metrics.set[i])
          p
      }
    graphics.off()
    rm(p,i)

  ##### Plot Confusion matrix #####
      ## Extract the confusion matrix
        cm <- cm.lt[["Predict1"]]

      ## Draw Confusion matrix
        source("Fun_Draw_ConfuMax.R")
        draw_confusion_matrix(cm)
        Draw_CM(cm)

        ## Full version
          pdf(
            file = paste0(Save.Path,"/",ProjectName,"_ConfuMax.pdf"),
            width = 17,  height = 12
          )
            for (i in 1:length(cm.lt)) {

              Draw_CM(cm.lt[[i]],names(cm.lt[i]))
            }
          #dev.off()
          graphics.off()
          rm(i)

        ## Full version
          pdf(
            file = paste0(Save.Path,"/",ProjectName,"_ConfuMaxSimp.pdf"),
            width = 10,  height = 7
          )
          for (i in 1:length(cm.lt)) {

            draw_confusion_matrix(cm.lt[[i]],names(cm.lt[i]))
          }
          #dev.off()
          graphics.off()
          rm(i)


#########################################################################################################
  ##### Misclassification rate #####
    Check_Bi.df$Correctness1 <- ""
    for (i in 1:nrow(Check_Bi.df)) {
      if(Check_Bi.df$Predict1[i] == Check_Bi.df$Actual[i] ){
        Check_Bi.df$Correctness1[i] = 0
      }else{
        Check_Bi.df$Correctness1[i] = 1
      }

    }

    # MissRate: Misclassification rate
    MissRate <- sum(Check_Bi.df$Correctness1 == 1)/nrow(Check_Bi.df)


    # Multi
    ## Ref: https://www.researchgate.net/figure/Confusion-matrix-for-60-training-and-40-testing-strategy_fig4_338909223


#########################################################################################################
    # RMSD: https://www.rdocumentation.org/packages/DescTools/versions/0.99.44/topics/Measures%20of%20Accuracy
    # RMSD: https://www.rdocumentation.org/packages/CDM/versions/7.5-15/topics/IRT.RMSD

