#' A function for creating summarize results of continuous data.
#'
#' This function allows you to create summarize results from continuous data.
#' @param Conti.df.df A dataframe of continuous answers and results.
#' @param Anno.df  A dataframe of annotation.
#' @param Mode one of 'One' or 'Multiple'. Chose Mode = "One" will export the result of the chosen index by the setting of BarMetricSet.lt, and chose Mode = "Multiple" will export all indexes and integrated results.
#' @param BarMetricSet.lt Set the index for subsequent comparison in Mode = "one".
#' @param Save.Path The setting of the saving path.Defaults to the path of the scripts folder.
#' @param ProjectName The naming of project Name.
#' @keywords Summarize results of continuous data.
#' @export
#' @examples
#' CellCheck_Conti(Conti.df = Sum_Conti.df, Anno.df = Simu_Anno.df,
#'                 Mode = "Multiple", BarMetricSet.lt,
#'                 Save.Path="", ProjectName="")
#'

CellCheck_Conti <- function(Conti.df = Sum_Conti.df, Anno.df = Simu_Anno.df,
                            Mode = "Multiple", # Mode = c("One","Multiple")
                            BarMetricSet.lt,
                            Save.Path="", ProjectName="") {
  library(DescTools)

  # Sum_Conti.df <- Conti_Accuracy(Simu_Conti.df$Actual,Simu_Conti.df$Predict2)

  #### Create Measure accuracy dataframe (Sum_Conti.df) ####
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

  #### Export MetricBar PDF ####

  if(Mode == "One"){
    #### Export one Designated MetricBar PDF ####
    ## Plot by Designated Metric
    p1 <- CC_BarPlot(Sum_Conti.df,
                     XValue = BarMetricSet.lt[["XValue"]],
                     Metrics = BarMetricSet.lt[["Metrics"]],
                     Group = BarMetricSet.lt[["Group"]])
    ## Export MetricBar PDF
    pdf(file = paste0(Save.Path,"/",ProjectName,"_Conti_MetricsBar_",BarMetricSet.lt[["Metrics"]],".pdf"),
        width = 7,  height = 7
    )
    p1 %>% print()
    dev.off()

    rm(p1)

  }else{
    #### Export all MetricBar PDF ####
    MA.set <- colnames(Sum_Conti.df)[2:(ncol(Sum_Conti.df)-ncol(Simu_Anno.df)+1)]
    pdf(file = paste0(Save.Path,"/",ProjectName,"_Conti_MetricsBar.pdf"),
        width = 7,  height = 7)
    for (i in 1:length(MA.set)) {
      p <- CC_BarPlot(Sum_Conti.df,
                      XValue = BarMetricSet.lt[["XValue"]],
                      Metrics = MA.set[i],
                      Group = BarMetricSet.lt[["Group"]])
      p %>% print()
    }
    dev.off()  #graphics.off()
    p %>% print()

    rm(p,i,BarMetricSet.lt,MA.set)


    #### Export MetricsLine PDF ####
    Sum_Conti.df$PARM <- factor(Sum_Conti.df$PARM,levels = sort(seq(1:15), decreasing = TRUE))

    #### Export one Designated MetricLine PDF ####
    ## Plot by Designated Metrics
    BarMetricSet.lt <- list(XValue = "PARM", Metrics = "RMSE", Group = "Tool")
    p <- CC_LinePlot(Sum_Conti.df,
                     XValue = BarMetricSet.lt[["XValue"]],
                     Metrics = BarMetricSet.lt[["Metrics"]],
                     Group = BarMetricSet.lt[["Group"]])
    rm(p)

    #### Export all MetricLine PDF ####
    MA.set <- colnames(Sum_Conti.df)[2:(ncol(Sum_Conti.df)-ncol(Simu_Anno.df)+1)]

    pdf(file = paste0(Save.Path,"/",ProjectName,"_Conti_MetricsLine.pdf"),
        width = 12,  height = 7)
    for (i in 1:length(MA.set)) {
      p <- CC_LinePlot(Sum_Conti.df,
                       XValue = BarMetricSet.lt[["XValue"]],
                       Metrics = MA.set[i],
                       Group = BarMetricSet.lt[["Group"]])
      p %>% print()
    }
    dev.off() #graphics.off()
    rm(p,i,BarMetricSet.lt)

    #### Export tsv files ####
    write.table(Sum_Conti.df, file=paste0(Save.Path,"/",ProjectName,"_Sum_Conti.tsv"),sep="\t",
                row.names=F, quote = FALSE)

  }
  return(Sum_Conti.df)
}



