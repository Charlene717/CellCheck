
CellCheck_Bi <- function(Simu_Bi.df, Simu_Anno.df, Mode = "One", CMPredSet.lt, # Mode = c("One","Multiple")
                         Save.Path="", ProjectName="") { 
  #####-----------------------------------(Binary data)-----------------------------------#####
  #### Calculate the confusion matrix(CM) ####
  library(caret)
  
  #### For one prediction ####
  # ## Set two comparisons
  # CMPredSet.lt <- list(Actual = "Actual", Predict = "Predict2")
  
  if(Mode == "One"){
    ## Build CM
    cm_Bi <- confusionMatrix(data = Simu_Bi.df[,CMPredSet.lt[["Actual"]]] %>% as.factor(),
                             reference = Simu_Bi.df[,CMPredSet.lt[["Predict"]]] %>% as.factor())
    
    ## Plot CM
    Bi_CMPlot(cm_Bi)%>% print()
    Bi_CMPlotSim(cm_Bi) %>% print()
    
    ## Export CM PDF
    pdf(file = paste0(Save.Path,"/",ProjectName,"_Bi_ConfuMax_",CMPredSet.lt[["Predict"]],".pdf"),
        width = 17,  height = 12
    )
    Bi_CMPlot(cm_Bi) %>% print()
    Bi_CMPlotSim(cm_Bi) %>% print()
    dev.off()
    
    return(cm_Bi)
    
  }else{
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
    Sum_Bi.df <- Bi_SummarizeCM(cm_Bi.lt, Simu_Anno.df)
    
    
    #### Export all CM PDF ####
    ## Full version
    pdf(
      file = paste0(Save.Path,"/",ProjectName,"_Bi_ConfuMax.pdf"),
      width = 17,  height = 12
    )
    for (i in 1:length(cm_Bi.lt)) {
      
      Bi_CMPlot(cm_Bi.lt[[i]],names(cm_Bi.lt[i])) %>% print()
    }
    dev.off() #graphics.off()
    rm(i)
    
    ## Simple version
    pdf(
      file = paste0(Save.Path,"/",ProjectName,"_Bi_ConfuMaxSimp.pdf"),
      width = 12,  height = 8
    )
    for (i in 1:length(cm_Bi.lt)) {
      
      Bi_CMPlotSim(cm_Bi.lt[[i]],names(cm_Bi.lt[i])) %>% print()
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
    p1 %>% print()
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
      p %>% print()
    }
    dev.off() # graphics.off()
    rm(p, i, Metrics_Bi.set, BarMetricSet.lt)
    
    
    #### Export MetricsLine PDF ####
    Sum_Bi.df$PARM <- factor(Sum_Bi.df$PARM,levels = sort(seq(1:15), decreasing = TRUE))
    
    #### Export one Designated MetricLine PDF ####
    ## Plot by Designated Metric
    LineMetricSet.lt <- list(XValue = "PARM", Metrics = "Accuracy", Group = "Tool")
    p <- CC_LinePlot(Sum_Bi.df, XValue = LineMetricSet.lt[["XValue"]],
                     Metrics = LineMetricSet.lt[["Metrics"]],
                     Group = LineMetricSet.lt[["Group"]])
    
    ## Export MetricBar PDF
    pdf(file = paste0(Save.Path,"/",ProjectName,"_Bi_MetricsLine_",LineMetricSet.lt[["Metrics"]],".pdf"),
        width = 12,  height = 7
    )
    p %>% print()
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
      p %>% print()
    }
    dev.off() #graphics.off()
    rm(p, i, LineMetricSet.lt, Sum_Bi.set)
    
    #### Export tsv files ####
    write.table(Sum_Bi.df, file=paste0(Save.Path,"/",ProjectName,"_Sum_Bi.tsv"),sep="\t",
                row.names=F, quote = FALSE)
    return(Sum_Bi.df)
  }

  
}

