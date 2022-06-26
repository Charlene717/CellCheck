#' A function for creating summarize results of binary data.
#'
#' This function allows you to create summarize results from binary data.
#' @param Bi.df A binary dataframe of answers and results.
#' @param Anno.df  A dataframe of annotation.
#' @param CMSet.lt Confusion matrix setting.
#' Mode = c("One","Multiple"), Mode = "One": Prediction selection; Mode = "Multiple": All Predictions;
#' Actual: Correct answer setting; Predict: Prediction selection in Mode = "One"; Remark = PDF file name setting.
#' @param BarChartSet.lt Bar Chart setting.
#' Mode = c("One","Multiple"), Mode = "One": Metric selection for y-axis setting; Mode = "Multiple": All Metrics;
#' Metric: Metric selection in Mode = "One" for y-axis setting; XValue: x-axis setting; Group: Group setting; Remark = PDF file name setting.
#' @param LinePlotSet.lt Line Plot setting.
#' Mode = c("One","Multiple"), Mode = "One": Metric selection for y-axis setting; Mode = "Multiple": All Metrics;
#' Metric: Metric selection in Mode = "One" for y-axis setting; XValue: x-axis setting; Group: Group setting; Remark = PDF file name setting.
#' @param Save.Path The setting of the saving path.Defaults to the path of the scripts folder.
#' @param ProjectName The naming of project Name.
#' @keywords Summarize results of binary data.
#' @export
#' @examples
#' CellCheck_Bi(Bi.df, Anno.df,
#'              CMSet.lt = CMSet.lt,
#'              BarChartSet.lt = BarChartSet.lt,
#'              LinePlotSet.lt = LinePlotSet.lt,
#'              Save.Path="", ProjectName="")
#'


CellCheck_Bi <- function(Bi.df , Anno.df,
                         CMSet.lt = "", # CMSet.lt = list(Mode = "Multiple", Actual = "Actual", Predict = "Predict2" , Remark = "Predict2") # Mode = c("One","Multiple")
                         BarChartSet.lt = "", # BarChartSet.lt = list(Mode = "Multiple", Metrics = "Accuracy", XValue = "Type", Group = "Tool", Remark = "")
                         LinePlotSet.lt = "", # LinePlotSet.lt = list(Mode = "Multiple", Metrics = "Accuracy", XValue = "PARM", Group = "Tool", Remark = "")
                         Save.Path="", ProjectName="") {

  ##### Load Packages #####
    ### Basic installation
    Package.set <- c("caret")
    ## Check whether the installation of those packages is required from basic
    for (i in 1:length(Package.set)) {
      if (!requireNamespace(Package.set[i], quietly = TRUE)){
        install.packages(Package.set[i])
      }
    }
    ## Load Packages
    lapply(Package.set, library, character.only = TRUE)
    rm(Package.set,i)

  ##### Build Confusion matrix(CM) #####
    ## Build list for all CM
    cm_Bi.lt <- list()
    for (i in 1:(ncol(Bi.df)-1)) {
      ## Build CM
      cm_Bi.lt[[i]] <- confusionMatrix(data = Bi.df[,1+i] %>% as.factor(),
                                       reference = Bi.df[,1] %>% as.factor())
      names(cm_Bi.lt)[[i]] <- colnames(Bi.df)[i+1]
    }
    rm(i)

    ## Build summarize dataframe
    SumCM_Bi.df <- Bi_SummarizeCM(cm_Bi.lt, Anno.df)

  ##### Plot CM #####
  ### For one prediction
  if(CMSet.lt[["Mode"]] == "One"){
    ## Build CM
    cm_Bi <- confusionMatrix(data = Bi.df[,CMSet.lt[["Predict"]]] %>% as.factor(),
                             reference = Bi.df[,CMSet.lt[["Actual"]]] %>% as.factor())

    ## Plot CM
    Bi_CMPlot(cm_Bi)%>% print()
    Bi_CMPlotSim(cm_Bi) %>% print()

    ## Export CM PDF
    pdf(file = paste0(Save.Path,"/",ProjectName,"_Bi_ConfuMax",CMSet.lt[["Remark"]],".pdf"),
        width = 17,  height = 12
    )
      Bi_CMPlot(cm_Bi) %>% print()
      Bi_CMPlotSim(cm_Bi) %>% print()
    dev.off()

  ## For all prediction
  }else{
  ## Export all CM PDF
    ## Full version
    pdf(
      file = paste0(Save.Path,"/",ProjectName,"_Bi_ConfuMax",CMSet.lt[["Remark"]],".pdf"),
      width = 17,  height = 12
    )
    for (i in 1:length(cm_Bi.lt)) {

      Bi_CMPlot(cm_Bi.lt[[i]],names(cm_Bi.lt[i])) %>% print()
    }
    dev.off() #graphics.off()
    rm(i)

    ## Simple version
    pdf(
      file = paste0(Save.Path,"/",ProjectName,"_Bi_ConfuMaxSimp",CMSet.lt[["Remark"]],".pdf"),
      width = 12,  height = 8
    )
    for (i in 1:length(cm_Bi.lt)) {

      Bi_CMPlotSim(cm_Bi.lt[[i]],names(cm_Bi.lt[i])) %>% print()
    }
    dev.off() #graphics.off()
    rm(i)
  }


  ##### Bar Chart #####
  if(length(BarChartSet.lt) > 1){
    ### For one Metric
    if(BarChartSet.lt[["Mode"]] == "One"){

      p1 <- CC_BarPlot(SumCM_Bi.df,
                       Metrics = BarChartSet.lt[["Metrics"]],
                       XValue = BarChartSet.lt[["XValue"]],
                       Group = BarChartSet.lt[["Group"]])

      ## Export MetricBar PDF
      pdf(file = paste0(Save.Path,"/",ProjectName,"_Bi_MetricsBar",BarChartSet.lt[["Metrics"]], BarChartSet.lt[["Remark"]],".pdf"),
          width = 7,  height = 7
      )
        p1 %>% print()
        dev.off()
      rm(p1)

  ### For all Metric
    }else{
      ## Export all MetricBar PDF
      Metrics_Bi.set <- colnames(SumCM_Bi.df)[2:(ncol(SumCM_Bi.df)-(ncol(Anno.df)-1))]

      pdf(file = paste0(Save.Path,"/",ProjectName,"_Bi_MetricsBar", BarChartSet.lt[["Remark"]],".pdf"),
          width = 7,  height = 7)
      for (i in 1:length(Metrics_Bi.set)) {
        p <- CC_BarPlot(SumCM_Bi.df,
                        XValue = BarChartSet.lt[["XValue"]],
                        Metrics = Metrics_Bi.set[i],
                        Group = BarChartSet.lt[["Group"]])
        p
      }
      dev.off() # graphics.off()
      rm(p, i, Metrics_Bi.set, BarChartSet.lt)

    }
  }

  ##### Line Plot #####
  if(length(LinePlotSet.lt) > 1){
    ### For one Metric
    if(LinePlotSet.lt[["Mode"]] == "One"){
      p <- CC_LinePlot(SumCM_Bi.df, XValue = LinePlotSet.lt[["XValue"]],
                       Metrics = LinePlotSet.lt[["Metrics"]],
                       Group = LinePlotSet.lt[["Group"]])
      ## Export Line plot PDF
      pdf(file = paste0(Save.Path,"/",ProjectName,"_Bi_MetricsLine",LinePlotSet.lt[["Metrics"]],LinePlotSet.lt[["Remark"]],".pdf"),
          width = 12,  height = 7
      )
        p %>% print()
      dev.off()
      rm(p)

    }else{
      #### Export all MetricLine PDF ####
      Sum_Bi.set <- colnames(SumCM_Bi.df)[2:(ncol(SumCM_Bi.df)-ncol(Anno.df)+1)]
      pdf(file = paste0(Save.Path,"/",ProjectName,"_Bi_MetricsLine",LinePlotSet.lt[["Remark"]],".pdf"),
          width = 12,  height = 7)
      for (i in 1:length(Sum_Bi.set)) {
        p <- CC_LinePlot(SumCM_Bi.df, XValue = LinePlotSet.lt[["XValue"]],
                         Metrics = Sum_Bi.set[i],
                         Group = LinePlotSet.lt[["Group"]])
        p
      }
      dev.off() #graphics.off()
      rm(p, i, LinePlotSet.lt, Sum_Bi.set)

    }
  }


  ##### Export Output #####
  if(CMSet.lt[["Mode"]] != "One"){
    #### Export tsv files ####
    write.table(SumCM_Bi.df, file=paste0(Save.Path,"/",ProjectName,"_Sum_Bi.tsv"),sep="\t",
                row.names=F, quote = FALSE)
    return(SumCM_Bi.df)

  }else{
    return(cm_Bi)
  }


}

