#' A function for plotting Barplot from dataframe
#'
#' This function allows you to plot Barplot from dataframe.
#' @param Results.df A dataframe for plotting Barplot.
#' @param XValue X-axis setting.
#' @param Metrics Y-axis setting. Usually represent the metrics.
#' @param Group Group setting.
#' @keywords Barplot
#' @export
#' @examples
#' CC_BarPlot(Results.df, XValue = "Type", Metrics = "Accuracy", Group = "Tool")
#'

# Ref(Bar Chart): https://officeguide.cc/r-ggplot2-bar-plot-tutorial-examples/
# Ref(Color): http://rstudio-pubs-static.s3.amazonaws.com/5312_98fc1aba2d5740dd849a5ab797cc2c8d.html


CC_BarPlot <- function(Results.df, XValue = "Type", Metrics = "Accuracy", Group = "Tool") {

  Xvalue = which(colnames(Results.df) == XValue)
  metrics = which(colnames(Results.df) == Metrics)
  group = which(colnames(Results.df) == Group)

  ## Plot by group
  p <- ggplot(data = Results.df, aes(x = Results.df[,Xvalue], y = Results.df[,metrics],
                                     fill = Results.df[,group]))+
    geom_bar(stat = "identity", position = position_dodge(), color="black",lwd=0.7)+
    theme_bw()+theme(panel.grid=element_blank(),panel.border=element_blank(),
                     axis.line=element_line(size=0,colour="black")) # White background and remove grid

  p2 <- p + scale_fill_brewer(palette = "Spectral")+ # scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9"))
        labs(#title = colnames(Results.df)[metrics],  # Change title in ggplot
          x = colnames(Results.df)[Xvalue],      # Change title of x axis in ggplot
          y = colnames(Results.df)[metrics],       # Change title of y axis in ggplot
          fill= colnames(Results.df)[group]     # Change legend title in ggplot
        )+
        theme(panel.border = element_rect(fill=NA,color="black", size= 2, linetype="solid"))+ # Outline
        theme(axis.text.x = element_text(color="black",face="bold",  size = 17,angle = 45, hjust = 1, vjust = .99), # Change the size along the x axis
              axis.text.y = element_text(color="black",face="bold",size = 17), # Change the size along the y axis

              # axis.line = element_line(colour = "darkblue", size = 2, linetype = "solid"),
              # axis.title = element_text(size = rel(2),face="bold",color = "#3d3d3d"),
              axis.title.x = element_text(size = rel(2),face="bold",color = "#1f1f1f", vjust = .2),
              axis.title.y = element_text(size = rel(2),face="bold",color = "#1f1f1f", vjust = 1.5),

              plot.title = element_text(color="black",
                                        size=20,
                                        face="bold.italic",
                                        hjust = 0.05,vjust =-10), # margin = margin(t = 0.5, b = -7),
              # plot.background = element_rect(fill = 'chartreuse'),
              legend.title = element_text(size=20, color = "black", face="bold", vjust = 1),
              legend.text = element_text(colour="black", size= 12,face="bold",
                                         margin = margin(l = -5, unit = "pt")),
              legend.key.size = unit(1.5, 'lines'),
              legend.background = element_rect(fill = alpha("white", 0.5)),
              # legend.position = c(0.1, 0.18),
              # plot.text = element_text(size = 20),
              aspect.ratio=1)   #square plot
  print(p2)

  return(p2)
}
