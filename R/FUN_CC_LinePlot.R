#' A function for plotting Lineplot from dataframe
#'
#' This function allows you to plot Lineplot from dataframe.
#' @param Results.df A dataframe for plotting Lineplot.
#' @param XValue X-axis setting.
#' @param Metrics Y-axis setting. Usually represent the Metrics.
#' @param Group Group setting.
#' @keywords Lineplot
#' @export
#' @examples
#' CC_Lineplot(Results.df, XValue = "Type", Metrics = "Accuracy", Group = "Tool")
#'


# Ref(Line Chart):
# Ref(Color): http://rstudio-pubs-static.s3.amazonaws.com/5312_98fc1aba2d5740dd849a5ab797cc2c8d.html


CC_LinePlot <- function(Results.df, XValue = "PARM", Metrics = "RMSE", Group = "Type") {


  ## Plot by Group
  p <- ggplot(Results.df, aes(x = Results.df[,XValue],
                              y = Results.df[,Metrics], group = Results.df[,Group])) +
    geom_line(aes(color = Results.df[,Group]), size = 2, alpha = 0.8)+
    geom_point(aes(color= Results.df[,Group], shape = Results.df[,Group]), size = 5, alpha = 0.8)+
    theme_bw() + theme(panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank())+
    scale_colour_brewer(palette = "Set1")


  p2 <- p + # scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9"))
    labs(#title = colnames(Results.df)[Metrics],  # Change title in ggplot
      x = XValue,      # Change title of x axis in ggplot
      y = Metrics,       # Change title of y axis in ggplot
      color = Group,     # Change legend title in ggplot
      shape = Group     # Change legend title in ggplot
    )+
    theme(panel.border = element_rect(fill=NA,color="black", size= 2.5, linetype="solid"))+ # Outline
    theme(axis.text.x = element_text(color="black",face="bold",  size = 17,angle = 0, hjust = 1, vjust = .99), # Change the size along the x axis
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
          aspect.ratio=0.5)   #square plot
  print(p2)

  return(p2)
}
