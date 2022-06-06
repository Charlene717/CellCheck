#' A function for creating summarize results of multiple discrete data.
#'
#' This function allows you to create summarize results from multiple discrete data.
#' @param DisMult.df A multiple discrete dataframe of answers and results.
#' @param Anno.df  A dataframe of annotation.
#' @param Mode one of 'One' or 'Multiple'. Chose Mode = "One" will export the confusion matrix of chosen prediction result by the setting of DisMultCM.lt, and chose Mode = "Multiple" will export all predictions in the dataframe and integrated results.
#' @param DisMultCM.lt Set correct answers and predicted results for subsequent comparison in Mode = "one".
#' @param Save.Path The setting of the saving path.Defaults to the path of the scripts folder.
#' @param ProjectName The naming of project Name.
#' @keywords Summarize results of multiple discrete data.
#' @export
#' @examples
#' CellCheck_Bi(Simu_DisMult.df, Simu_Anno.df,
#'              Mode = "Multiple", DisMultCM.lt,
#'              Save.Path="", ProjectName="")
#'



##### calculate the confusion matrix for Multi-Class Classification #####
## Ref: https://www.researchgate.net/figure/Confusion-matrix-for-60-training-and-40-testing-strategy_fig4_338909223
## Ref: https://cran.r-project.org/web/packages/cvms/vignettes/Creating_a_confusion_matrix.html

CellCheck_DisMult <- function(Simu_DisMult.df, Simu_Anno.df, Mode = "Multiple", DisMultCM.lt, # Mode = c("One","Multiple")
                              Save.Path="", ProjectName="") {
  library(cvms)

  if(Mode == "One"){
    #### For one prediction ####
    # ## Set two comparisons
    # DisMultCM.lt <- list(Actual = "Actual", Predict = "Predict2")

    conf_mat <- confusion_matrix(targets = Simu_DisMult.df[,DisMultCM.lt[["Actual"]]], # Simu_DisMult.df$Actual
                                 predictions = Simu_DisMult.df[,DisMultCM.lt[["Predict"]]])

    conf_mat


    pdf(
      file = paste0(Save.Path,"/",ProjectName,"_DisMult_ConfuMax_",DisMultCM.lt[["Predict"]],".pdf"),
      width = 10,  height = 10
    )
    p1 <- plot_confusion_matrix(conf_mat$`Confusion Matrix`[[1]],
                                add_sums = TRUE)

    p1 + ggtitle(DisMultCM.lt[["Predict"]])+
      theme(axis.text.x = element_text(face="bold", color="#3d3d3d", size=12), #plot.margin = unit(c(2,3,3,4),"cm")
            axis.text.y = element_text(face="bold", color="#3d3d3d", size=12,angle=0),
            axis.title.x = element_text(face="bold", color="#3d3d3d", size=18),
            axis.title.y = element_text(face="bold", color="#3d3d3d", size=18),
            title = element_text(face="bold", color="#3d3d3d", size=18)) -> p2
    print(p2)
    dev.off()

    print(p2)
    rm(p1,p2)

    return(conf_mat)

  }else{
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
    # Sum_DisMult_All.df <- Sum_DisMult_All.df[Sum_DisMult_All.df$Tool == "ToolA",]
    Sum_DisMult_All.df <- Sum_DisMult_All.df[Sum_DisMult_All.df[,DisMultCM.lt[["Type1"]]] == DisMultCM.lt[["Type2"]],]

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
    write.table(Sum_DisMult_All.df[,-4:-5], file=paste0(Save.Path,"/",ProjectName,"_Sum_DisMult_All.tsv"),sep="\t",
                row.names=F, quote = FALSE)

    #### Build summarize dataframe ####
    for (i in 1:length(conf_mat.lt)) {
      if(i==1){
        Sum_DisMult2.df <- data.frame(TestID =  names(conf_mat.lt[i]),
                                      conf_mat.lt[[i]] %>% as.data.frame())
      }else{
        Sum_DisMult2_New.df <- data.frame(TestID =  names(conf_mat.lt[i]),
                                          conf_mat.lt[[i]] %>% as.data.frame())
        Sum_DisMult2.df <- rbind(Sum_DisMult2.df, Sum_DisMult2_New.df)
      }
    }

    rm(i,Sum_DisMult2_New.df)
    ## Add annotation dataframe
    Sum_DisMult2.df <- left_join(Sum_DisMult2.df, Simu_Anno.df)

    #### Export all MetricBar PDF ####
    Metrics_DisMult.set <- colnames(Sum_DisMult2.df)[5:(ncol(Sum_DisMult2.df)-(ncol(Simu_Anno.df)-1))]
    BarMetricSet.lt <- list(XValue = "Type", Metrics = "Balanced.Accuracy", Group = "Tool")
    pdf(file = paste0(Save.Path,"/",ProjectName,"_DisMult_MetricsBar.pdf"),
        width = 7,  height = 7)
    for (i in 1:length(Metrics_DisMult.set)) {
      p <- CC_BarPlot(Sum_DisMult2.df,
                      XValue = BarMetricSet.lt[["XValue"]],
                      Metrics = Metrics_DisMult.set[i],
                      Group = BarMetricSet.lt[["Group"]])
      p
    }
    dev.off() # graphics.off()
    rm(p, i, BarMetricSet.lt, Metrics_DisMult.set)
    #### Export MetricLine PDF ####
    Sum_DisMult2.df$PARM <- factor(Sum_DisMult2.df$PARM,levels = sort(seq(1:15), decreasing = TRUE))

    ## Plot by Designated Metric
    LineMetricSet.lt <- list(XValue = "PARM", Metrics = "Balanced.Accuracy", Group = "Tool")
    p <- CC_LinePlot(Sum_DisMult2.df, XValue = LineMetricSet.lt[["XValue"]],
                     Metrics = LineMetricSet.lt[["Metrics"]],
                     Group = LineMetricSet.lt[["Group"]])

    #### Export all MetricLine PDF ####
    Metrics_DisMult.set <- colnames(Sum_DisMult2.df)[5:(ncol(Sum_DisMult2.df)-(ncol(Simu_Anno.df)-1))]

    pdf(file = paste0(Save.Path,"/",ProjectName,"_DisMult_MetricsLine.pdf"),
        width = 10,  height = 7)
    for (i in 1:length(Metrics_DisMult.set)) {
      p <- CC_LinePlot(Sum_DisMult2.df,
                       XValue = LineMetricSet.lt[["XValue"]],
                       Metrics = Metrics_DisMult.set[i],
                       Group = LineMetricSet.lt[["Group"]])
      p
    }
    dev.off() # graphics.off()
    rm(p,i,LineMetricSet.lt)

    #### Export tsv files ####
    write.table(Sum_DisMult2.df[,-2:-4], file=paste0(Save.Path,"/",ProjectName,"_Sum_DisMult.tsv"),sep="\t",
                row.names=F, quote = FALSE)

    return(Sum_DisMult2.df)
  }

}





