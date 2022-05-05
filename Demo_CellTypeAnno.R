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
  source("Fun_Draw_ConfuMatrix.R")
  source("Fun_Summary_CM.R")


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
                         Predict3 = sample(c(0,1), 100, replace = TRUE),
                         Predict4 = sample(c(0,1), 100, replace = TRUE),
                         Predict5 = sample(c(0,1), 100, replace = TRUE),
                         Predict6 = sample(c(0,1), 100, replace = TRUE))

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


##### Compare Multigroup #####

    cm.lt <- list()

    for (i in 1:(ncol(Check.df)-1)) {
      cm.lt[[i]] <- confusionMatrix(data = Check.df[,1] %>% as.factor(),
                            reference = Check.df[,1+i] %>% as.factor())
      names(cm.lt)[[i]] <- colnames(Check.df)[i+1]

    }
    rm(i)


    ## Result dataframe
      # Results.df <- ""
      for (i in 1:length(cm.lt)) {

          if(i==1){
            Results.df <- data.frame( cm.lt[[i]][["overall"]] )
            colnames(Results.df)[i] <- names(cm.lt[i])
            Results.df <- Results.df %>% t() %>% as.data.frame()

          }else{
            Results_S.df <- data.frame( cm.lt[[i]][["overall"]] )
            colnames(Results_S.df) <- names(cm.lt[i])
            Results_S.df <- Results_S.df %>% t() %>% as.data.frame()
            Results.df <- rbind(Results.df,Results_S.df)
          }
      }

      rm(i,Results_S.df)
      cm.lt[["Predict1"]][["overall"]][["Accuracy"]]

      Results.df <- data.frame(Test=row.names(Results.df),Results.df)

      ## (Pending) Create df
      Results.df$Tool <- c("A","A","A","B","B","B")
      Results.df$Type <- c("1","2","3","1","2","3")
      ## Test function
      Results.df2 <- Summary_CM(cm.lt)

    ## Plot Result
      # Ref(Bar Chart): https://officeguide.cc/r-ggplot2-bar-plot-tutorial-examples/
      # Ref(Color): http://rstudio-pubs-static.s3.amazonaws.com/5312_98fc1aba2d5740dd849a5ab797cc2c8d.html

      ## Plot without grouping
      p <- ggplot(data = Results.df, aes(x = Results.df[,1], y = Results.df[,2],
                                         fill = Results.df[,1]))+
                  geom_bar(stat = "identity")

      p + scale_fill_brewer(palette = "Dark2")+ # scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9"))
          labs(title = colnames(Results.df)[2],
               x = colnames(Results.df)[1],
               y = colnames(Results.df)[2],
               fill= colnames(Results.df)[1]) # Change legend title in ggplot

      ## Plot by group
      p2 <- ggplot(data = Results.df, aes(x = Type, y = Results.df[,2],
                                          fill = Tool))+
        geom_bar(stat = "identity", position = position_dodge())

      p2+ scale_fill_brewer(palette = "Dark2")+ # scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9"))
          labs(#title = colnames(Results.df)[2],
               #x = colnames(Results.df)[1],
               y = colnames(Results.df)[2],
               #fill= colnames(Results.df)[1] # Change legend title in ggplot
               )


#########################################################################################################
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



##########################################################
    # Multi
    ## Ref: https://www.researchgate.net/figure/Confusion-matrix-for-60-training-and-40-testing-strategy_fig4_338909223



