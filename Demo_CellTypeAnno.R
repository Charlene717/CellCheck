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
      p <- ggplot(data = Results.df, aes(x = Type, y = Results.df[,2],
                                          fill = Tool))+
        geom_bar(stat = "identity", position = position_dodge(), color="black",lwd=1.2)+
        theme_bw()+theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour="black")) # White background and remove grid

      p + scale_fill_brewer(palette = "Spectral")+ # scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9"))
          labs(#title = colnames(Results.df)[2],  # Change title in ggplot
               #x = colnames(Results.df)[1],      # Change title of x axis in ggplot
               y = colnames(Results.df)[2],       # Change title of y axis in ggplot
               #fill= colnames(Results.df)[1]     # Change legend title in ggplot
               )+
          theme(panel.border = element_rect(fill=NA,color="black", size= 2.5, linetype="solid"))+ # Outline
          theme(axis.text.x = element_text(color="black",face="bold",  size = 17,angle = 45, hjust = 1, vjust = .99), # Change the size along the x axis
                axis.text.y = element_text(color="black",face="bold",size = 17), # Change the size along the y axis

                #axis.line = element_line(colour = "darkblue", size = 2, linetype = "solid"),
                # axis.title = element_text(size = rel(2),face="bold",color = "#3d3d3d"),
                axis.title.x = element_text(size = rel(2),face="bold",color = "#3d3d3d", vjust = .2),
                axis.title.y = element_text(size = rel(2),face="bold",color = "#3d3d3d", vjust = 1.5),

                plot.title = element_text(color="black",
                                          size=20,
                                          face="bold.italic",
                                          hjust = 0.05,vjust =-10), # margin = margin(t = 0.5, b = -7),
                #     plot.background = element_rect(fill = 'chartreuse'),
                legend.title = element_text(size=20, color = "black", face="bold"),
                legend.text = element_text(colour="black", size= 12,face="bold"),
                legend.background = element_rect(fill = alpha("white", 0.5)),
                #      legend.position = c(0.1, 0.18),
                #     plot.text = element_text(size = 20),
                aspect.ratio=1)   #square plot

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



