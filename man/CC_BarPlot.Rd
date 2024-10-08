% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FUN_CC_BarPlot.R
\name{CC_BarPlot}
\alias{CC_BarPlot}
\title{A function for plotting Barplot from dataframe}
\usage{
CC_BarPlot(Results.df, XValue = "Type", Metrics = "Accuracy", Group = "Tool")
}
\arguments{
\item{Results.df}{A dataframe for plotting Barplot.}

\item{XValue}{X-axis setting.}

\item{Metrics}{Y-axis setting. Usually represent the Metrics.}

\item{Group}{Group setting.}
}
\description{
This function allows you to plot Barplot from dataframe.
}
\examples{
CC_BarPlot(Results.df, XValue = "Type", Metrics = "Accuracy", Group = "Tool")

}
\keyword{Barplot}
