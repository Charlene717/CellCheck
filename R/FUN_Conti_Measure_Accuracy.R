#' A function for creating Measure accuracy dataframe
#'
#' This function allows you to create dataframe of measure accuracy from continuous data .
#' @param Actual A serous of actual value.
#' @param Predict A serous of predict value.
#' @keywords summarize the confusion matrix
#' @export
#' @examples
#' Measure_Accuracy(Actual, Predict,  NaRm = FALSE)
#'


## Ref: https://www.rdocumentation.org/packages/DescTools/versions/0.99.44/topics/Measures%20of%20Accuracy
## Ref: https://www.rdocumentation.org/packages/CDM/versions/7.5-15/topics/IRT.RMSD

## Ref: https://rpubs.com/ivan0628/numerical_model_evaluation#:~:text=%E5%B9%B3%E5%9D%87%E7%B5%95%E5%B0%8D%E7%99%BE%E5%88%86%E8%AA%A4%E5%B7%AE(MAPE%2C%20Mean%20Absolute%20Percentage%20Error)&text=%E6%98%AF%E5%8F%96%E7%9B%B8%E5%B0%8D%E8%AA%A4%E5%B7%AE(e,%E6%A8%A1%E5%9E%8B%E7%82%BA%E5%8F%AF%E6%8E%A5%E5%8F%97%E7%9A%84%E3%80%82

Measure_Accuracy <- function(Actual, Predict,  NaRm = FALSE) {

  ## Check whether the installation of the package is required
  if (!requireNamespace("DescTools", quietly = TRUE)){
    install.packages("DescTools")
  }
  library(DescTools)

  ## Create Measure accuracy dataframe (MA.df)
  MA.df  <- data.frame(
                        MAE = MAE(x = Predict, ref = Actual, na.rm = NaRm),
                        MAPE = MAPE(x = Predict, ref = Actual, na.rm = NaRm),
                        SMAPE = SMAPE(x = Predict, ref = Actual, na.rm = NaRm),
                        MSE = MSE(x = Predict, ref = Actual, na.rm = NaRm),
                        RMSE = RMSE(x = Predict, ref = Actual, na.rm = NaRm)
                      )

  return(MA.df)
}
