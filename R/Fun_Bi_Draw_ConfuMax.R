#' A function for drawing confusion matrix in simple version
#'
#' This function allows you to draw confusion matrix for binary data in simple version.
#' @param cm Dataframe of confusion matrix.
#' @param TestID Title name of the plot.
#' @keywords confusion matrix
#' @export
#' @examples
#' Draw_Bi_CM(cm,TestID = "Predict1")


## https://stackoverflow.com/questions/23891140/r-how-to-visualize-confusion-matrix-using-the-caret-package
## @Cybernetic  ## https://stackoverflow.com/users/1639594/cybernetic

draw_confusion_matrix <- function(cm,TestID = "Predict1") {

  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  box(lwd=2)
  title(paste0('CONFUSION MATRIX'," (",TestID,") "), cex.main=2.5)

  # create the matrix
  rect(135, 430, 225, 370, col='#324d75')
  text(180, 435, 'Condition positive', cex=2, font=2)
  rect(230, 430, 325, 370, col='#434047')
  text(277, 435, 'Condition negative', cex=2, font=2)
  text(110, 370, 'Predicted', cex=3, srt=90, font=2)
  text(230, 445, 'Actual', cex=3, font=2)
  rect(135, 305, 225, 365, col='#434047')
  rect(230, 305, 325, 365, col='#324d75')
  text(125, 400, 'Condition \n positive', cex=2, font=2, srt=90)
  text(125, 335, 'Condition \n negative', cex=2, font=2, srt=90)

  # add in the cm results
  res <- as.numeric(cm$table)
  text(180, 400, paste0("TP = ", res[1]), cex=3, font=2, col='white')
  text(180, 335, paste0("FN = ", res[2]), cex=3, font=2, col='white')
  text(280, 400, paste0("FP = ", res[3]), cex=3, font=2, col='white')
  text(280, 335, paste0("TN = ", res[4]), cex=3, font=2, col='white')

  # add in the specifics
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n', cex.main=2.5)
  box(lwd=2)
  text(10, 85, names(cm$byClass[1]), cex=2, font=2)
  text(10, 70, round(as.numeric(cm$byClass[1]), 3), cex=2)
  text(30, 85, names(cm$byClass[2]), cex=2, font=2)
  text(30, 70, round(as.numeric(cm$byClass[2]), 3), cex=2)
  text(50, 85, names(cm$byClass[5]), cex=2, font=2)
  text(50, 70, round(as.numeric(cm$byClass[5]), 3), cex=2)
  text(70, 85, names(cm$byClass[6]), cex=2, font=2)
  text(70, 70, round(as.numeric(cm$byClass[6]), 3), cex=2)
  text(90, 85, names(cm$byClass[7]), cex=2, font=2)
  text(90, 70, round(as.numeric(cm$byClass[7]), 3), cex=2)

  # add in the accuracy information
  text(30, 35, names(cm$overall[1]), cex=3, font=2)
  text(30, 15, round(as.numeric(cm$overall[1]), 3), cex=2)
  text(70, 35, names(cm$overall[2]), cex=3, font=2)
  text(70, 15, round(as.numeric(cm$overall[2]), 3), cex=2)
}
