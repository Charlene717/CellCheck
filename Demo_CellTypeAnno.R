##### To Do List ######
## Discrete data: Binary data
  # - [ ] Confusion matrix (Simple version)
  # - [ ] Confusion matrix (Full version)
  # - [ ] Annotation table
  # - [ ] Compare different conditions

## Discrete data: Multiple data
  # - [ ] Confusion matrix
  # - [ ] Annotation table
  # - [ ] Compare different conditions

## Continuous data



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

##### Compare Multigroup #####

    cm.lt <- list()

    for (i in 1:(ncol(Check_Bi.df)-1)) {
      cm.lt[[i]] <- confusionMatrix(data = Check_Bi.df[,1] %>% as.factor(),
                            reference = Check_Bi.df[,1+i] %>% as.factor())
      names(cm.lt)[[i]] <- colnames(Check_Bi.df)[i+1]

    }
    rm(i)


    ## Result dataframe
      Results.df <- Summary_CM(cm.lt, Anno.df)

    ## Plot Result
      # Ref(Bar Chart): https://officeguide.cc/r-ggplot2-bar-plot-tutorial-examples/
      # Ref(Color): http://rstudio-pubs-static.s3.amazonaws.com/5312_98fc1aba2d5740dd849a5ab797cc2c8d.html

      ## Plot by group
      p <- Plot_CMBar(Results.df, Metrics = "Accuracy")
      p

##### Confusion matrix #####
    #### calculate the confusion matrix ####
      library(caret)
      # cm <- confusionMatrix(data = Check_Bi.df$Actual %>% as.factor(),
      #                       reference = Check_Bi.df$Predict1 %>% as.factor())
      cm <- cm.lt[["Predict1"]]

    #### Draw Confusion Matrix ####
      source("Fun_Draw_ConfuMax.R")
      draw_confusion_matrix(cm)
      Draw_CM(cm)

      pdf(
        file = paste0(Save.Path,"/",ProjectName,"_ConfuMax.pdf"),
        width = 12,  height = 12
      )
      Draw_CM(cm)

      dev.off()


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



##########################################################
    # Multi
    ## Ref: https://www.researchgate.net/figure/Confusion-matrix-for-60-training-and-40-testing-strategy_fig4_338909223



