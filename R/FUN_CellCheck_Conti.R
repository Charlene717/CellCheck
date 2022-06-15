#' A function for creating summarize results of continuous data.
#'
#' This function allows you to create summarize results from continuous data.
#' @param Conti.df.df A dataframe of continuous answers and results.
#' @param Anno.df  A dataframe of annotation.
#' @param BarChartSet.lt Bar Chart setting.
#' Mode = c("One","Multiple"), Mode = "One": Metric selection for y-axis setting; Mode = "Multiple": All Metrics;
#' Metric: Metric selection in Mode = "One" for y-axis setting; XValue: x-axis setting; Group: Group setting; Remark = PDF file name setting.
#' @param LinePlotSet.lt Line Plot setting.
#' Mode = c("One","Multiple"), Mode = "One": Metric selection for y-axis setting; Mode = "Multiple": All Metrics;
#' Metric: Metric selection in Mode = "One" for y-axis setting; XValue: x-axis setting; Group: Group setting; Remark = PDF file name setting.
#' @param Save.Path The setting of the saving path.Defaults to the path of the scripts folder.
#' @param ProjectName The naming of project Name.
#' @keywords Summarize results of continuous data.
#' @export
#' @examples
#' CellCheck_Conti(Conti.df = Sum_Conti.df, Anno.df = Simu_Anno.df,
#'                 BarChartSet.lt = BarChartSet.lt,
#'                 LinePlotSet.lt =  LinePlotSet.lt,
#'                 Save.Path="", ProjectName="")
#'

CellCheck_Conti <- function(Conti.df = Sum_Conti.df, Anno.df = Simu_Anno.df,
                            BarChartSet.lt = "", # BarChartSet.lt = list(Mode = "Multiple", Metrics = "Accuracy", XValue = "Type", Group = "Tool", Remark = "") # Mode = c("One","Multiple")
                            LinePlotSet.lt = "", # LinePlotSet.lt = list(Mode = "Multiple", Metrics = "Accuracy", XValue = "PARM", Group = "Tool", Remark = "")
                            Save.Path="", ProjectName="") {

  ##### Load Packages #####
  ### Basic installation
  Package.set <- c("DescTools")
  ## Check whether the installation of those packages is required from basic
  for (i in 1:length(Package.set)) {
    if (!requireNamespace(Package.set[i], quietly = TRUE)){
      install.packages(Package.set[i])
    }
  }
  ## Load Packages
  lapply(Package.set, library, character.only = TRUE)
  rm(Package.set,i)

  ##### Create Measure accuracy dataframe (Sum_Conti.df) #####
  # Sum_Conti.df <- Conti_Accuracy(Simu_Conti.df$Actual,Simu_Conti.df$Predict2)
  for (i in 1:(ncol(Simu_Conti.df)-1)) {
    if(i==1){
      Sum_Conti.df <- Conti_Accuracy(Simu_Conti.df[,1],
                                     Simu_Conti.df[,1+i])
      row.names(Sum_Conti.df) <- colnames(Simu_Conti.df)[i+1]
    }else{
      MA_New.df <- Conti_Accuracy(Simu_Conti.df[,1],
                                  Simu_Conti.df[,1+i])
      row.names(MA_New.df) <- colnames(Simu_Conti.df)[i+1]
      Sum_Conti.df <- rbind(Sum_Conti.df, MA_New.df)
    }
  }
  rm(i,MA_New.df)

  Sum_Conti.df <- data.frame(TestID = row.names(Sum_Conti.df), Sum_Conti.df)
  Sum_Conti.df <- left_join(Sum_Conti.df, Simu_Anno.df)

  ##### Bar Chart #####
  ### For one prediction
  if(length(BarChartSet.lt) > 1){
    ### For one Metric
    if(BarChartSet.lt[["Mode"]] == "One"){
      ## Export one Designated MetricBar PDF
      p1 <- CC_BarPlot(Sum_Conti.df,
                       XValue = BarChartSet.lt[["XValue"]],
                       Metrics = BarChartSet.lt[["Metrics"]],
                       Group = BarChartSet.lt[["Group"]])
      ## Export MetricBar PDF
      pdf(file = paste0(Save.Path,"/",ProjectName,"_Conti_MetricsBar_",BarChartSet.lt[["Metrics"]], BarChartSet.lt[["Remark"]],".pdf"),
          width = 7,  height = 7
      )
      p1 %>% print()
      dev.off()

      rm(p1)

    ### For all Metric
    }else{
      ## Export all MetricBar PDF
      MA.set <- colnames(Sum_Conti.df)[2:(ncol(Sum_Conti.df)-ncol(Simu_Anno.df)+1)]
      pdf(file = paste0(Save.Path,"/",ProjectName,"_Conti_MetricsBar", BarChartSet.lt[["Remark"]],".pdf"),
          width = 7,  height = 7)
      for (i in 1:length(MA.set)) {
        p <- CC_BarPlot(Sum_Conti.df,
                        XValue = BarChartSet.lt[["XValue"]],
                        Metrics = MA.set[i],
                        Group = BarChartSet.lt[["Group"]])
        p
      }
      dev.off()  #graphics.off()
      p %>% print()

      rm(p,i,BarChartSet.lt,MA.set)

    }
  }

  ##### Line Plot #####
  ### For one prediction
  if(length(LinePlotSet.lt) > 1){
    ### For one Metric
    if(LinePlotSet.lt[["Mode"]] == "One"){
      ## Export MetricsLine PDF
       ## Plot by Designated Metrics
      p <- CC_LinePlot(Sum_Conti.df,
                       XValue = LinePlotSet.lt[["XValue"]],
                       Metrics = LinePlotSet.lt[["Metrics"]],
                       Group = LinePlotSet.lt[["Group"]])
      ## Export MetricBar PDF
      pdf(file = paste0(Save.Path,"/",ProjectName,"_Conti_MetricsLine_",LinePlotSet.lt[["Metrics"]], LinePlotSet.lt[["Remark"]],".pdf"),
          width = 7,  height = 7
      )
        p %>% print()
      dev.off()

      rm(p)


    ### For all Metric
    }else{
      ## Export all MetricBar PDF
      MA.set <- colnames(Sum_Conti.df)[2:(ncol(Sum_Conti.df)-ncol(Simu_Anno.df)+1)]
      pdf(file = paste0(Save.Path,"/",ProjectName,"_Conti_MetricsLine", LinePlotSet.lt[["Remark"]],".pdf"),
          width = 7,  height = 7)
      for (i in 1:length(MA.set)) {
        p <- CC_LinePlot(Sum_Conti.df,
                        XValue = LinePlotSet.lt[["XValue"]],
                        Metrics = MA.set[i],
                        Group = LinePlotSet.lt[["Group"]])
        p
      }
      dev.off()  #graphics.off()
      p %>% print()

      rm(p,i,MA.set)

    }
  }



  #### Export tsv files ####
  write.table(Sum_Conti.df, file=paste0(Save.Path,"/",ProjectName,"_Sum_Conti.tsv"),sep="\t",
              row.names=F, quote = FALSE)

  return(Sum_Conti.df)
}



