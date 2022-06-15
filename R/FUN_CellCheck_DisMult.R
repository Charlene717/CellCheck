#' A function for creating summarize results of multiple discrete data.
#'
#' This function allows you to create summarize results from multiple discrete data.
#' @param DisMult.df A multiple discrete dataframe of answers and results.
#' @param Anno.df  A dataframe of annotation.
#' @param Mode one of 'One' or 'Multiple'. Chose Mode = "One" will export the confusion matrix of chosen prediction result by the setting of DisCMSet.lt, and chose Mode = "Multiple" will export all predictions in the dataframe and integrated results.
#' @param DisCMSet.lt Set correct answers and predicted results for subsequent comparison in Mode = "one".
#' @param Save.Path The setting of the saving path.Defaults to the path of the scripts folder.
#' @param ProjectName The naming of project Name.
#' @keywords Summarize results of multiple discrete data.
#' @export
#' @examples
#' CellCheck_DisMult(Simu_DisMult.df, Simu_Anno.df,
#'                   DisCMSet.lt  = DisCMSet.lt ,
#'                   BarChartSet.lt = BarChartSet.lt,
#'                   LinePlotSet.lt = LinePlotSet.lt,
#'                   Save.Path="", ProjectName="")
#'



##### calculate the confusion matrix for Multi-Class Classification #####
## Ref: https://www.researchgate.net/figure/Confusion-matrix-for-60-training-and-40-testing-strategy_fig4_338909223
## Ref: https://cran.r-project.org/web/packages/cvms/vignettes/Creating_a_confusion_matrix.html

CellCheck_DisMult <- function(Simu_DisMult.df, Simu_Anno.df,
                              DisCMSet.lt = "", # DisCMSet.lt = list(Mode = "Multiple", Actual = "Actual", Predict = "Predict2" , CTChose1 = "Type", CTChose2 = "LUAD" , Remark = "Predict2") # Mode = c("One","Multiple")
                              BarChartSet.lt = "", # BarChartSet.lt = list(Mode = "Multiple", Metrics = "Accuracy", XValue = "Type", Group = "Tool", Remark = "")
                              LinePlotSet.lt = "", # LinePlotSet.lt = list(Mode = "Multiple", Metrics = "Accuracy", XValue = "PARM", Group = "Tool", Remark = "")
                              Save.Path="", ProjectName=""){

  ##### Load Packages #####
  ### Basic installation
  Package.set <- c("cvms")
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

    #### Build summarize dataframe for all cell type ####
    ## Build list for all CM
    conf_mat.lt <- list()
    for (i in 1:(ncol(Simu_DisMult.df)-1)) {
      conf_mat.lt[[i]] <- confusion_matrix(targets = Simu_DisMult.df[,"Actual"] %>% as.factor(),
                                           predictions = Simu_DisMult.df[,1+i] %>% as.factor())
      names(conf_mat.lt)[[i]] <- colnames(Simu_DisMult.df)[i+1]

    }
    rm(i)

    ## Build summarize dataframe for all condition
    for (i in 1:length(conf_mat.lt)) {
      if(i==1){
        SumCM_DisMult_All.df <- data.frame(TestID =  names(conf_mat.lt[i]),
                                           conf_mat.lt[[i]][["Class Level Results"]] %>% as.data.frame())
      }else{
        Sum_DisMult_All_New.df <- data.frame(TestID =  names(conf_mat.lt[i]),
                                             conf_mat.lt[[i]][["Class Level Results"]] %>% as.data.frame())
        SumCM_DisMult_All.df <- rbind(SumCM_DisMult_All.df, Sum_DisMult_All_New.df)
      }
    }

    rm(i,Sum_DisMult_All_New.df)
    ## Add annotation dataframe
    SumCM_DisMult_All.df <- left_join(SumCM_DisMult_All.df, Simu_Anno.df)


    #### Build summarize dataframe ####
    for (i in 1:length(conf_mat.lt)) {
      if(i==1){
        SumCM_DisMult.df <- data.frame(TestID =  names(conf_mat.lt[i]),
                                       conf_mat.lt[[i]] %>% as.data.frame())
      }else{
        Sum_DisMult2_New.df <- data.frame(TestID =  names(conf_mat.lt[i]),
                                          conf_mat.lt[[i]] %>% as.data.frame())
        SumCM_DisMult.df <- rbind(SumCM_DisMult.df, Sum_DisMult2_New.df)
      }
    }

    rm(i,Sum_DisMult2_New.df)
    ## Add annotation dataframe
    SumCM_DisMult.df <- left_join(SumCM_DisMult.df, Simu_Anno.df)

    ## Create Output
    SumCM_DisMult.lt <- list(SumCM_DisMult.df = SumCM_DisMult.df,
                             SumCM_DisMult_All.df = SumCM_DisMult_All.df)


    ## Remove in the future
    # SumCM_DisMult_All.df <- SumCM_DisMult_All.df[SumCM_DisMult_All.df$Tool == "ToolA",]
    if(DisCMSet.lt[["CTChose1"]] != ""){
      SumCM_DisMult_All.df <- SumCM_DisMult_All.df[SumCM_DisMult_All.df[,DisCMSet.lt[["CTChose1"]]] == DisCMSet.lt[["CTChose2"]],]
    }

  ##### Plot CM #####
  ### For one prediction
  if(DisCMSet.lt[["Mode"]] == "One"){
    ## Build CM
    conf_mat <- confusion_matrix(targets = Simu_DisMult.df[,DisCMSet.lt[["Actual"]]], # Simu_DisMult.df$Actual
                                 predictions = Simu_DisMult.df[,DisCMSet.lt[["Predict"]]])

    conf_mat

    p1 <- plot_confusion_matrix(conf_mat$`Confusion Matrix`[[1]],
                                add_sums = TRUE)

    pdf(
      file = paste0(Save.Path,"/",ProjectName,"_DisMult_ConfuMax_",DisCMSet.lt[["Predict"]],DisCMSet.lt[["Remark"]],".pdf"),
      width = 10,  height = 10
    )


      p1 + ggtitle(DisCMSet.lt[["Predict"]])+
        theme(axis.text.x = element_text(face="bold", color="#3d3d3d", size=12), #plot.margin = unit(c(2,3,3,4),"cm")
              axis.text.y = element_text(face="bold", color="#3d3d3d", size=12,angle=0),
              axis.title.x = element_text(face="bold", color="#3d3d3d", size=18),
              axis.title.y = element_text(face="bold", color="#3d3d3d", size=18),
              title = element_text(face="bold", color="#3d3d3d", size=18)) -> p2
      print(p2)
    dev.off()

    print(p2)
    rm(p1,p2)



  ## For all predictions
  }else{

  ## Export PDF of Multi-Class Classification CM
    pdf(
      file = paste0(Save.Path,"/",ProjectName,"_DisMult_ConfuMax",DisCMSet.lt[["Remark"]],".pdf"),
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

  }

  ##### Bar Chart #####
    #### Plot with all cell type ####
    if(length(BarChartSet.lt) > 1){
      ### For one Metric
      if(BarChartSet.lt[["Mode"]] == "One"){
        p1 <- CC_BarPlot(SumCM_DisMult_All.df,
                         XValue = BarChartSet.lt[["XValue"]],
                         Metrics = BarChartSet.lt[["Metrics"]],
                         Group = BarChartSet.lt[["Group"]])
        ## Export MetricBar PDF
        pdf(file = paste0(Save.Path,"/",ProjectName,"_DisMult_MetricsBar_AllCT_",BarChartSet.lt[["Metrics"]], BarChartSet.lt[["Remark"]],".pdf"),
            width = 7,  height = 7
        )
          p1 %>% print()
          dev.off()
        rm(p1)

      ### For all Metric all
      }else{
        ## Plot all MetricBar
        Metrics_DisMult.set <- colnames(SumCM_DisMult_All.df)[6:(ncol(SumCM_DisMult_All.df)-(ncol(Simu_Anno.df)-1))]
        pdf(file = paste0(Save.Path,"/",ProjectName,"_DisMult_MetricsBar_AllCT",BarChartSet.lt[["Remark"]],".pdf"),
            width = 7,  height = 7)
        for (i in 1:length(Metrics_DisMult.set)) {
          p <- CC_BarPlot(SumCM_DisMult_All.df,
                          XValue = BarChartSet.lt[["XValue"]],
                          Metrics = Metrics_DisMult.set[i],
                          Group = BarChartSet.lt[["Group"]])
          p
        }
        dev.off() # graphics.off()
        rm(p,i,BarChartSet.lt)

      }
    }

    #### Plot with summarize ####
    if(length(BarChartSet.lt) > 1){
      ### For one Metric
      if(BarChartSet.lt[["Mode"]] == "One"){
        p1 <- CC_BarPlot(SumCM_DisMult.df,
                         XValue = BarChartSet.lt[["XValue"]],
                         Metrics = BarChartSet.lt[["Metrics"]],
                         Group = BarChartSet.lt[["Group"]])
        ## Export MetricBar PDF
        pdf(file = paste0(Save.Path,"/",ProjectName,"_DisMult_MetricsBar_",BarChartSet.lt[["Metrics"]], BarChartSet.lt[["Remark"]],".pdf"),
            width = 7,  height = 7
        )
        p1 %>% print()
        dev.off()
        rm(p1)

        ### For all Metric all
      }else{
        ## Plot all MetricBar
        Metrics_DisMult.set <- colnames(SumCM_DisMult.df)[6:(ncol(SumCM_DisMult.df)-(ncol(Simu_Anno.df)-1))]
        pdf(file = paste0(Save.Path,"/",ProjectName,"_DisMult_MetricsBar",BarChartSet.lt[["Remark"]],".pdf"),
            width = 7,  height = 7)
        for (i in 1:length(Metrics_DisMult.set)) {
          p <- CC_BarPlot(SumCM_DisMult.df,
                          XValue = BarChartSet.lt[["XValue"]],
                          Metrics = Metrics_DisMult.set[i],
                          Group = BarChartSet.lt[["Group"]])
          p
        }
        dev.off() # graphics.off()
        rm(p,i,BarChartSet.lt)
      }

    }

  ##### Line Plot #####
    #### Plot with all cell type ####
    if(length(LinePlotSet.lt) > 1){
      ### For one Metric
      if(LinePlotSet.lt[["Mode"]] == "One"){
        p1 <- CC_LinePlot(SumCM_DisMult_All.df, XValue = LinePlotSet.lt[["XValue"]],
                         Metrics = LinePlotSet.lt[["Metrics"]],
                         Group = LinePlotSet.lt[["Group"]])
        ## Export MetricBar PDF
        pdf(file = paste0(Save.Path,"/",ProjectName,"_DisMult_MetricsLine_AllCT_",LinePlotSet.lt[["Metrics"]], LinePlotSet.lt[["Remark"]],".pdf"),
            width = 7,  height = 7
        )
          p1 %>% print()
          dev.off()
        rm(p1)

        ### For all Metric all
      }else{
        ## Plot all MetricBar
        Metrics_DisMult.set <- colnames(SumCM_DisMult_All.df)[6:(ncol(SumCM_DisMult_All.df)-(ncol(Simu_Anno.df)-1))]

        pdf(file = paste0(Save.Path,"/",ProjectName,"_DisMult_MetricsLine_AllCT", LinePlotSet.lt[["Remark"]],".pdf"),
            width = 10,  height = 7)
          for (i in 1:length(Metrics_DisMult.set)) {
            p <- CC_LinePlot(SumCM_DisMult_All.df,
                             XValue = LinePlotSet.lt[["XValue"]],
                             Metrics = Metrics_DisMult.set[i],
                             Group = LinePlotSet.lt[["Group"]])
            p
          }
        dev.off() # graphics.off()
        rm(p,i,LinePlotSet.lt)

      }
    }



    #### Plot with summarize ####
    if(length(LinePlotSet.lt) > 1){
      ### For one Metric
      if(LinePlotSet.lt[["Mode"]] == "One"){
        p1 <- CC_LinePlot(SumCM_DisMult.df, XValue = LinePlotSet.lt[["XValue"]],
                          Metrics = LinePlotSet.lt[["Metrics"]],
                          Group = LinePlotSet.lt[["Group"]])
        ## Export MetricBar PDF
        pdf(file = paste0(Save.Path,"/",ProjectName,"_DisMult_MetricsLine_",LinePlotSet.lt[["Metrics"]], LinePlotSet.lt[["Remark"]],".pdf"),
            width = 7,  height = 7
        )
        p1 %>% print()
        dev.off()
        rm(p1)

        ### For all Metric all
      }else{
        ## Plot all MetricBar
        Metrics_DisMult.set <- colnames(SumCM_DisMult.df)[6:(ncol(SumCM_DisMult.df)-(ncol(Simu_Anno.df)-1))]

        pdf(file = paste0(Save.Path,"/",ProjectName,"_DisMult_MetricsLine", LinePlotSet.lt[["Remark"]],".pdf"),
            width = 10,  height = 7)
        for (i in 1:length(Metrics_DisMult.set)) {
          p <- CC_LinePlot(SumCM_DisMult.df,
                           XValue = LinePlotSet.lt[["XValue"]],
                           Metrics = Metrics_DisMult.set[i],
                           Group = LinePlotSet.lt[["Group"]])
          p
        }
        dev.off() # graphics.off()
        rm(p,i,LinePlotSet.lt)

      }
    }

    ##### Export Output #####
    if(DisCMSet.lt[["Mode"]] != "One"){
      ## Export tsv files
      write.table(SumCM_DisMult.df[,-2:-4], file=paste0(Save.Path,"/",ProjectName,"_Sum_DisMult.tsv"),sep="\t",
                  row.names=F, quote = FALSE)
      write.table(SumCM_DisMult_All.df[,-4:-5], file=paste0(Save.Path,"/",ProjectName,"_Sum_DisMult_All.tsv"),sep="\t",
                  row.names=F, quote = FALSE)

      return(SumCM_DisMult.lt)

    }else{
      return(conf_mat)
    }
}

