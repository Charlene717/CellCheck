
##### Presetting ######
  rm(list = ls()) # Clean variable
  memory.limit(150000)

##### Load Packages #####
  ## Check whether the installation of those packages is required
  Package.set <- c("tidyverse","caret","cvms","DescTools","devtools","ggthemes")
  for (i in 1:length(Package.set)) {
    if (!requireNamespace(Package.set[i], quietly = TRUE)){
      install.packages(Package.set[i])
    }
  }
  ## Load Packages
  # library(Seurat)
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


##### Load simulation datafrme #####
  load("Create_simulation_datafrme.RData")

#####-----------------------------------(Binary data)-----------------------------------#####
#### Calculate the confusion matrix(CM) ####
  library(caret)

  #### For one prediction ####
  ## Set two comparisons
  CMPredSet.lt <- list(Actual = "Actual",
                       Predict = "Predict2")

  ## Build CM
  cm_Bi <- confusionMatrix(data = Simu_Bi.df[,CMPredSet.lt[["Actual"]]] %>% as.factor(),
                           reference = Simu_Bi.df[,CMPredSet.lt[["Predict"]]] %>% as.factor())

  ## Plot CM
  p1 <- Draw_Bi_CM(cm_Bi)
  p2 <- draw_confusion_matrix(cm_Bi)

  ## Export CM PDF
  pdf(file = paste0(Save.Path,"/",ProjectName,"_Bi_ConfuMax_",CMPredSet.lt[["Predict"]],".pdf"),
      width = 17,  height = 12
  )
    Draw_Bi_CM(cm_Bi)
    draw_confusion_matrix(cm_Bi)
  dev.off()
  rm(p1,p2,CMPredSet.lt,cm_Bi)

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
    # cm_Bi <- cm_Bi.lt[["Predict2"]]
    #
    # ## Draw Confusion matrix
    # draw_confusion_matrix(cm_Bi)
    # Draw_Bi_CM(cm_Bi)
    # rm(cm_Bi)

  #### Export all CM PDF ####
    ## Full version
    pdf(
      file = paste0(Save.Path,"/",ProjectName,"_Bi_ConfuMax.pdf"),
      width = 17,  height = 12
    )
    for (i in 1:length(cm_Bi.lt)) {

      Draw_Bi_CM(cm_Bi.lt[[i]],names(cm_Bi.lt[i]))
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
    #### Export one Designated MetricBar PDF ####
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

    #### Export one Designated MetricLine PDF ####
    ## Plot by Designated Metric
    LineMetricSet.lt <- list(XValue = "PARM", Metrics = "Accuracy", Group = "Type")
    p <- CC_LinePlot(Sum_Bi.df, XValue = LineMetricSet.lt[["XValue"]],
                     Metrics = LineMetricSet.lt[["Metrics"]],
                     Group = LineMetricSet.lt[["Group"]])

    ## Export MetricBar PDF
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

    #### Export tsv files ####
    write.table(Sum_Bi.df, file=paste0(Save.Path,"/",ProjectName,"_Sum_Bi.tsv"),sep="\t",
                row.names=F, quote = FALSE)

#####--------------------------(Discrete Multiple data)--------------------------#####
##### Calculate Accuracy(ACC) and Misclassification rate (Error Rate, ER) #####
  Sum_DisMult.df <- AccEr_DiscMult(Simu_DisMult.df, Simu_Anno.df)

  #### Export MetricBar PDF ####
    ## Plot one Designated MetricBar
    BarMetricSet.lt <- list(XValue = "Type", Metrics = "Accuracy", Group = "Tool")
    p1 <- CC_BarPlot(Sum_DisMult.df,
                     XValue = BarMetricSet.lt[["XValue"]],
                     Metrics = BarMetricSet.lt[["Metrics"]],
                     Group = BarMetricSet.lt[["Group"]])
    rm(p1)

    Metrics_DisMult.set <- colnames(Sum_DisMult.df)[2:(ncol(Sum_DisMult.df)-(ncol(Simu_Anno.df)-1))]

    #### Export all MetricBar PDF ####
    pdf(file = paste0(Save.Path,"/",ProjectName,"_DisMult_MetricsBar.pdf"),
        width = 7,  height = 7)
      for (i in 1:length(Metrics_DisMult.set)) {
        p <- CC_BarPlot(Sum_DisMult.df,
                        XValue = BarMetricSet.lt[["XValue"]],
                        Metrics = Metrics_DisMult.set[i],
                        Group = BarMetricSet.lt[["Group"]])
        p
      }
    dev.off() # graphics.off()
    rm(p,i,BarMetricSet.lt)

  #### Export MetricLine PDF ####
    Sum_DisMult.df$PARM <- factor(Sum_DisMult.df$PARM,levels = sort(seq(1:15), decreasing = TRUE))

    ## Plot by Designated Metric
    LineMetricSet.lt <- list(XValue = "PARM", Metrics = "Accuracy", Group = "Type")
    p <- CC_LinePlot(Sum_DisMult.df, XValue = LineMetricSet.lt[["XValue"]],
                     Metrics = LineMetricSet.lt[["Metrics"]],
                     Group = LineMetricSet.lt[["Group"]])

    #### Export all MetricLine PDF ####
    Metrics_DisMult.set <- colnames(Sum_DisMult.df)[2:(ncol(Sum_DisMult.df)-(ncol(Simu_Anno.df)-1))]

    pdf(file = paste0(Save.Path,"/",ProjectName,"_DisMult_MetricsLine.pdf"),
        width = 7,  height = 7)
    for (i in 1:length(Metrics_DisMult.set)) {
      p <- CC_LinePlot(Sum_DisMult.df,
                      XValue = LineMetricSet.lt[["XValue"]],
                      Metrics = Metrics_DisMult.set[i],
                      Group = LineMetricSet.lt[["Group"]])
      p
    }
    dev.off() # graphics.off()
    rm(p,i,LineMetricSet.lt)

    #### Export tsv files ####
    write.table(Sum_DisMult.df, file=paste0(Save.Path,"/",ProjectName,"_Sum_DisMult.tsv"),sep="\t",
                row.names=F, quote = FALSE)

  ##### calculate the confusion matrix for Multi-Class Classification #####
  ## Ref: https://www.researchgate.net/figure/Confusion-matrix-for-60-training-and-40-testing-strategy_fig4_338909223
  ## Ref: https://cran.r-project.org/web/packages/cvms/vignettes/Creating_a_confusion_matrix.html
    library(cvms)
    #### For one prediction ####
    DisMultCM.set <- "Predict2"
    conf_mat <- confusion_matrix(targets = Simu_DisMult.df$Actual,
                                 predictions = Simu_DisMult.df[,DisMultCM.set])

    conf_mat

    plot_confusion_matrix(conf_mat$`Confusion Matrix`[[1]],
                          add_sums = TRUE)
    pdf(
      file = paste0(Save.Path,"/",ProjectName,"_DisMult_ConfuMax_",DisMultCM.set,".pdf"),
      width = 10,  height = 10
    )
        p1 <- plot_confusion_matrix(conf_mat$`Confusion Matrix`[[1]],
                                    add_sums = TRUE)

        p1 + ggtitle(DisMultCM.set)+
          theme(axis.text.x = element_text(face="bold", color="#3d3d3d", size=12), #plot.margin = unit(c(2,3,3,4),"cm")
                axis.text.y = element_text(face="bold", color="#3d3d3d", size=12,angle=0),
                axis.title.x = element_text(face="bold", color="#3d3d3d", size=18),
                axis.title.y = element_text(face="bold", color="#3d3d3d", size=18),
                title = element_text(face="bold", color="#3d3d3d", size=18)) -> p2
        print(p2)
    dev.off()
    rm(p1,p2)

    #### For all predictions ####
      #### calculate the confusion matrix ####
      conf_mat.lt <- list()

      for (i in 1:(ncol(Simu_DisMult.df)-1)) {
        conf_mat.lt[[i]] <- confusion_matrix(targets = Simu_DisMult.df[,"Actual"] %>% as.factor(),
                                             predictions = Simu_DisMult.df[,1+i] %>% as.factor())
        names(conf_mat.lt)[[i]] <- colnames(Simu_DisMult.df)[i+1]

      }
      rm(i)

      #### Export PDF of Multi-Class Classification CM ####
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

      #### Build summarize dataframe for all condition ####
      for (i in 1:length(conf_mat.lt)) {
        if(i==1){
          Sum_DisMult_All.df <- data.frame(TestID =  names(conf_mat.lt[i]),
                                conf_mat.lt[[i]][["Class Level Results"]] %>% as.data.frame())
        }else{
          Sum_DisMult_All_New.df <- data.frame(TestID =  names(conf_mat.lt[i]),
                                               conf_mat.lt[[i]][["Class Level Results"]] %>% as.data.frame())
          Sum_DisMult_All.df <- rbind(Sum_DisMult_All.df, Sum_DisMult_All_New.df)
        }
      }

      rm(i,Sum_DisMult_All_New.df)
      ## Add annotation dataframe
      Sum_DisMult_All.df <- left_join(Sum_DisMult_All.df, Simu_Anno.df)

      ## Remove in the future
      Sum_DisMult_All.df <- Sum_DisMult_All.df[Sum_DisMult_All.df$Type == "Type1",]

      #### Export MetricBar PDF ####
      ## Plot one Designated MetricBar
      BarMetricSet.lt <- list(XValue = "Tool", Metrics = "Balanced.Accuracy", Group = "Class")
      p1 <- CC_BarPlot(Sum_DisMult_All.df,
                       XValue = BarMetricSet.lt[["XValue"]],
                       Metrics = BarMetricSet.lt[["Metrics"]],
                       Group = BarMetricSet.lt[["Group"]])
      rm(p1)

      Metrics_DisMult.set <- colnames(Sum_DisMult_All.df)[6:(ncol(Sum_DisMult_All.df)-(ncol(Simu_Anno.df)-1))]

      #### Export all MetricBar PDF ####
      pdf(file = paste0(Save.Path,"/",ProjectName,"_DisMult_MetricsBar_All.pdf"),
          width = 7,  height = 7)
      for (i in 1:length(Metrics_DisMult.set)) {
        p <- CC_BarPlot(Sum_DisMult_All.df,
                        XValue = BarMetricSet.lt[["XValue"]],
                        Metrics = Metrics_DisMult.set[i],
                        Group = BarMetricSet.lt[["Group"]])
        p
      }
      dev.off() # graphics.off()
      rm(p,i,BarMetricSet.lt)

      #### Export MetricLine PDF ####
      Sum_DisMult_All.df$PARM <- factor(Sum_DisMult_All.df$PARM,levels = sort(seq(1:15), decreasing = TRUE))

      ## Plot by Designated Metric
      LineMetricSet.lt <- list(XValue = "PARM", Metrics = "Balanced.Accuracy", Group = "Class")
      p <- CC_LinePlot(Sum_DisMult_All.df, XValue = LineMetricSet.lt[["XValue"]],
                       Metrics = LineMetricSet.lt[["Metrics"]],
                       Group = LineMetricSet.lt[["Group"]])

      #### Export all MetricLine PDF ####
      Metrics_DisMult.set <- colnames(Sum_DisMult_All.df)[6:(ncol(Sum_DisMult_All.df)-(ncol(Simu_Anno.df)-1))]

      pdf(file = paste0(Save.Path,"/",ProjectName,"_DisMult_MetricsLine_All.pdf"),
          width = 10,  height = 7)
      for (i in 1:length(Metrics_DisMult.set)) {
        p <- CC_LinePlot(Sum_DisMult_All.df,
                         XValue = LineMetricSet.lt[["XValue"]],
                         Metrics = Metrics_DisMult.set[i],
                         Group = LineMetricSet.lt[["Group"]])
        p
      }
      dev.off() # graphics.off()
      rm(p,i,LineMetricSet.lt)

      #### Export tsv files ####
      write.table(Sum_DisMult_All.df, file=paste0(Save.Path,"/",ProjectName,"_Sum_DisMult_All.tsv"),sep="\t",
                  row.names=F, quote = FALSE)



#####---------------------------------(Continuous data)---------------------------------#####

  library(DescTools)

  # MA.df <- Measure_Accuracy(Simu_Conti.df$Actual,Simu_Conti.df$Predict2)

  #### Create Measure accuracy dataframe (MA.df) ####
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

  #### Export MetricBar PDF ####
    #### Export one Designated MetricBar PDF ####
    ## Plot by Designated Metric
    BarMetricSet.lt <- list(XValue = "Type", Metrics = "RMSE", Group = "Tool")
    p1 <- CC_BarPlot(MA.df,
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
    MA.set <- colnames(MA.df)[2:(ncol(MA.df)-ncol(Simu_Anno.df)+1)]
    pdf(file = paste0(Save.Path,"/",ProjectName,"_Conti_MetricsBar.pdf"),
        width = 7,  height = 7)
      for (i in 1:length(MA.set)) {
        p <- CC_BarPlot(MA.df,
                        XValue = BarMetricSet.lt[["XValue"]],
                        Metrics = MA.set[i],
                        Group = BarMetricSet.lt[["Group"]])
        p
      }
    dev.off()  #graphics.off()
    rm(p,i,BarMetricSet.lt,MA.set)


  #### Export MetricsLine PDF ####
  MA.df$PARM <- factor(MA.df$PARM,levels = sort(seq(1:15), decreasing = TRUE))

  #### Export one Designated MetricLine PDF ####
  ## Plot by Designated Metric
  BarMetricSet.lt <- list(XValue = "PARM", Metrics = "RMSE", Group = "Type")
  p <- CC_LinePlot(MA.df,
                   XValue = BarMetricSet.lt[["XValue"]],
                   Metrics = BarMetricSet.lt[["Metrics"]],
                   Group = BarMetricSet.lt[["Group"]])
  rm(p)

  #### Export all MetricLine PDF ####
  MA.set <- colnames(MA.df)[2:(ncol(MA.df)-ncol(Simu_Anno.df)+1)]

  pdf(file = paste0(Save.Path,"/",ProjectName,"_Conti_MetricsLine.pdf"),
      width = 12,  height = 7)
    for (i in 1:length(MA.set)) {
      p <- CC_LinePlot(MA.df,
                       XValue = "PARM",
                       Metrics = MA.set[i],
                       Group = "Type")
      p
    }
  dev.off() #graphics.off()
  rm(p,i)

  #### Export tsv files ####
  write.table(MA.df, file=paste0(Save.Path,"/",ProjectName,"_Sum_Conti.tsv"),sep="\t",
              row.names=F, quote = FALSE)

#### Export RData files ####
save.image(paste0(Save.Path,"/",ProjectName,"Demo_CellTypeAnno.RData"))


