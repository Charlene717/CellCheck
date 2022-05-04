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
  source("Fun_Draw_ConfuMatrix.R")


##### Load datasets #####


##### Current path and new folder setting*  #####
  ProjectName = "CC"

  Version = paste0(Sys.Date(),"_","CC_Demo")
  Save.Path = paste0(getwd(),"/",Version)
  dir.create(Save.Path)



##### Simulated datafrme #####
  Check.df <- data.frame(Actual = sample(c(0,1), 100, replace = TRUE),
                         Predict1 = sample(c(0,1), 100, replace = TRUE),
                         Predict2 = sample(c(0,1), 100, replace = TRUE),
                         Predict3 = sample(c(0,1), 100, replace = TRUE))

##### Confusion matrix #####
  #### calculate the confusion matrix ####
    library(caret)
    cm <- confusionMatrix(data = Check.df$Actual %>% as.factor(),
                          reference = Check.df$Predict1 %>% as.factor())


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

  ##### Misclassification rate #####
    Check.df$Correctness1 <- ""
    for (i in 1:nrow(Check.df)) {
      if(Check.df$Predict1[i] == Check.df$Actual[i] ){
        Check.df$Correctness1[i] = 0
      }else{
        Check.df$Correctness1[i] = 1
      }

    }

    # MissRate: Misclassification rate
    MissRate <- sum(Check.df$Correctness1 == 1)/nrow(Check.df)

