% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FUN_CC_LinePlot.R
\name{CC_LinePlot}
\alias{CC_LinePlot}
\title{A function for plotting Lineplot from dataframe}
\usage{
CC_LinePlot(Results.df, XValue = "PARM", Metrics = "RMSE", Group = "Type")
}
\arguments{
\item{Results.df}{A dataframe for plotting Lineplot.}

\item{XValue}{X-axis setting.}

\item{Metrics}{Y-axis setting. Usually represent the Metrics.}

\item{Group}{Group setting.}
}
\description{
This function allows you to plot Lineplot from dataframe.
}
\examples{
CC_Lineplot(Results.df, XValue = "Type", Metrics = "Accuracy", Group = "Tool")

}
\keyword{Lineplot}
