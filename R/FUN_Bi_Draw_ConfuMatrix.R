#' A function for drawing confusion matrix
#'
#' This function allows you to draw confusion matrix for binary data.
#' @param cm Dataframe of confusion matrix.
#' @param TestID Title name of the plot.
#' @keywords confusion matrix
#' @export
#' @examples
#' Bi_CMPlot(cm,TestID = "Predict1")


## Ref: https://stackoverflow.com/questions/23891140/r-how-to-visualize-confusion-matrix-using-the-caret-package
## @Cybernetic  ## https://stackoverflow.com/users/1639594/cybernetic

## Ref: https://towardsdatascience.com/confusion-matrix-deep-dive-8a028b005a97
## Ref: https://cran.r-project.org/web/packages/ConfusionTableR/vignettes/ConfusionTableR.html


Bi_CMPlot <- function(cm,TestID = "Predict1") {

  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(50, 350), c(220, 550), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  box(lwd=2)
  title(paste0('CONFUSION MATRIX'," (",TestID,") "), cex.main=2.1)

  # create the matrix
  rect(60, 530, 120, 460, col='#324d75') #7f55a6
  rect(60, 542, 120, 532, col='#0f274a')
  text(90, 537, 'Condition positive', cex=1.4, font=2, col='white')
  rect(121, 530, 181, 460, col='#434047')
  rect(121, 542, 181, 532, col='#0f274a')
  text(151, 537, 'Condition negative', cex=1.4, font=2, col='white')
  text(48, 470, 'Predicted', cex=2, srt=90, font=2)
  text(121, 550, 'Actual', cex=2, font=2)
  rect(60, 388, 120, 458, col='#434047')
  rect(121, 388, 181, 458, col='#324d75')
  rect(51, 530, 59, 460, col='#0f274a')
  rect(51, 388, 59, 458, col='#0f274a')
  text(55, 495, 'Condition \n positive', cex=1.4, srt=90, font=2, col='white')
  text(55, 423, 'Condition \n negative', cex=1.4, srt=90, font=2, col='white')

  # add in the cm results
  res <- as.numeric(cm$table)
  TP = res[1]
  FN = res[2]
  FP = res[3]
  TN = res[4]
  text(90, 495, paste0("True positive = ", res[1],"\n Power"), cex=1.8, font=2, col='white')
  text(90, 423, paste0("False negative = ",res[2],"\n Type II error"), cex=1.8, font=2, col='white')
  text(151, 495, paste0("False positive = ",res[3],"\n Type I error"), cex=1.8, font=2, col='white')
  text(151, 423, paste0("True negative = ",res[4]), cex=1.8, font=2, col='white')

  # create the sup matrix
  rect(60, 315, 120, 385, col='#6ca9bd')
  rect(60, 242, 120, 312, col='#6ca9bd')
  rect(121, 315, 181, 385, col='#6ca9bd')
  rect(121, 242, 181, 312, col='#6ca9bd')

  rect(182, 530, 242, 460, col='#a7c291')
  rect(182, 388, 242, 458, col='#a7c291')
  rect(243, 530, 303, 460, col='#a7c291')
  rect(243, 388, 303, 458, col='#a7c291')#819c6a

  rect(182, 315, 242, 385, col='#e6d67e')
  rect(182, 242, 242, 312, col='#e6d67e')
  rect(243, 242, 272, 385, col='#e6d67e')
  rect(273, 242, 303, 385, col='#e6d67e')



  # add in the results
  text(90, 365, paste0("TPR = ", round(cm[["byClass"]][["Sensitivity"]],4),
                       "\n True positive rate(TPR)",
                       "\n Recall,Sensitivity"),
       cex=1.4, font=2, col='#2a4c57')
  text(90, 337, expression(frac(TP,TP + FN)),cex=1.2, font=3, col='#2a4c57')

  text(151, 365, paste0("FPR = ", round(res[3]/(res[3]+res[4]),4),
                        "\n False positive rate(FPR)",
                        "\n Fall-out"),
       cex=1.4, font=2, col='#2a4c57')
  text(151, 337, expression(frac(FP,FP + TN)),cex=1.2, font=3, col='#2a4c57')

  text(90, 292, paste0("FNR = ", round(res[2]/(res[1]+res[2]),4),
                       "\n False negative rate(FNR)",
                       "\n Miss rate"),
       cex=1.4, font=2, col='#2a4c57')
  text(90, 264, expression(frac(FN,TP + FN)),cex=1.2, font=3, col='#2a4c57')

  text(151, 292, paste0("TNR = ", round(cm[["byClass"]][["Specificity"]],4),
                        "\n True negative rate(TNR)",
                        "\n Specificity(SPC)"),
       cex=1.4, font=2, col='#2a4c57')
  text(151, 264, expression(frac(TN,FP + TN)),cex=1.2, font=3, col='#2a4c57')

  text(212, 508, paste0("PPV = ", round(cm[["byClass"]][["Precision"]],4),
                        "\n Positive predictive value(PPV)",
                        "\n Precision"),
       cex=1.4, font=2, col='#506143')
  text(212, 480, expression(frac(TP,TP + FP)),cex=1.2, font=3, col='#506143')

  text(212, 437, paste0("FOR = ", round(res[2]/(res[2]+res[4]),4),
                        "\n False omission rate(FOR)",
                        "\n False omission rate"),
       cex=1.4, font=2, col='#506143')
  text(212, 409, expression(frac(FN,FN + TN)),cex=1.2, font=3, col='#506143')

  text(273, 508, paste0("FDR = ", round(res[3]/(res[1]+res[3]),4),
                        "\n False dicovery rate(FDR)"),
       cex=1.4, font=2, col='#506143')
  text(273, 480, expression(frac(FP,TP + FP)),cex=1.2, font=3, col='#506143')

  text(273, 437, paste0("NPV = ", round(res[4]/(res[2]+res[4]),4),
                        "\n Nagtive predictive value(NPV)"),
       cex=1.4, font=2, col='#506143')
  text(273, 409, expression(frac(TN,FN + TN)),cex=1.2, font=3, col='#506143')

  LRP = cm[["byClass"]][["Sensitivity"]]/(res[3]/(res[3]+res[4]))
  text(212, 365, paste0("LR+ = ", round(LRP,4),
                        "\n Positive likelihood ratio(LR+)"),
       cex=1.4, font=2, col='#524c2e')
  text(212, 337, expression(frac(TPR,FPR)),cex=1.2, font=3, col='#524c2e')

  LRN = (res[2]/(res[1]+res[2]))/cm[["byClass"]][["Specificity"]]
  text(212, 292, paste0("LR- = ",round(LRN,4),
                        "\n Positive likelihood ratio(LR-)"),
       cex=1.4, font=2, col='#524c2e')
  text(212, 264, expression(frac(FNR,TNR)),cex=1.2, font=3, col='#524c2e')

  text(258, 334, paste0("DOR = \n", round(LRP/LRN,4),
                        "\n \n Diagnostic odds \n ratio(DOR)"),
       cex=1.4, font=2, col='#524c2e')
  text(258, 286, expression(frac("LR+","LR-")),cex=1.2, font=3, col='#524c2e')


  text(288, 334, paste0("F1 score = \n", round(cm[["byClass"]][["F1"]],4),"\n "),
       cex=1.6, font=2, col='#524c2e')
  text(288, 306, expression(frac(2,frac(1,Recall)+frac(1,Precision))),cex=1.2, font=3, col='#524c2e')

  ##################################################################################
  ## Accuracy
  text(332, 550, paste0("Accuracy (ACC) = ",(res[1]+res[4])/(res[1]+res[2]+res[3]+res[4])),
                        cex=1.7, font=2)
  text(332, 530,expression(frac(TP+TN,'Total population')),cex=1.2, font=2)
  # text(330, 537, paste0("Prevalence = ",(res[1]+res[2])/(res[1]+res[2]+res[3]+res[4])),
  #     cex=2, font=2)

  ## Misclassification
  text(332, 510, paste0("Error Rate = ",(res[2]+res[3])/(res[1]+res[2]+res[3]+res[4])),
       cex=1.7, font=2)
  text(332, 490,expression(frac(FP+FN,'Total population')),cex=1.2, font=2)

  ## Prevalence
  text(332, 470, paste0("Prevalence = ",(res[1]+res[2])/(res[1]+res[2]+res[3]+res[4])),
       cex=1.7, font=2)
  text(332, 450,expression(frac(TP+FN,'Total population')),cex=1.2, font=2)

  # Ref: http://iccm.cc/classification-model-evaluation-confusion-matrix/
  ## Detection rate
  text(332, 430, paste0("Detection rate = ",round(cm[["byClass"]][["Detection Rate"]],4)),
       cex=1.7, font=2)
  text(332, 410,expression(frac(TP,'Total population')),cex=1.2, font=2)

  ## Detection Prevalence
  text(332, 390, paste0("Detection Prevalence = ",round(cm[["byClass"]][["Detection Prevalence"]],4)),
       cex=1.7, font=2)
  text(332, 370,expression(frac(TP+FP,'Total population')),cex=1.2, font=2)

  ## Balanced Accuracy = (Sensitivity+Specificity)/2
  text(332, 350, paste0("Balanced Accuracy = ",round(cm[["byClass"]][["Balanced Accuracy"]],4)),
       cex=1.7, font=2)
  text(332, 330,expression(frac(Sensitivity+Specificity,'2')),cex=1.2, font=2)

  ## Kappa
  text(332, 310, paste0("Kappa = ",round(cm[["overall"]][["Kappa"]],4)),
       cex=1.7, font=2)
  #text(332, 350,expression(frac(?????)),cex=1.7, font=2)

  ## McnemarPValue
  text(332, 295, paste0("McnemarPValue = ",round(cm[["overall"]][["McnemarPValue"]],4)),
       cex=1.7, font=2)
  ## AccuracyPValue
  text(332, 280, paste0("AccuracyPValue = ",round(cm[["overall"]][["AccuracyPValue"]],4)),
       cex=1.7, font=2)
  ## AccuracyNull
  text(332, 265, paste0("AccuracyNull = ",round(cm[["overall"]][["AccuracyNull"]],4)),
       cex=1.7, font=2)
  ## AccuracyUpper
  text(332, 250, paste0("AccuracyUpper = ",round(cm[["overall"]][["AccuracyUpper"]],4)),
       cex=1.7, font=2)
  ## AccuracyLower
  text(332, 235, paste0("AccuracyLower = ",round(cm[["overall"]][["AccuracyLower"]],4)),
       cex=1.7, font=2)
  ## Matthews correlation coefficient
  MCC = (TP*TN - FP*FN)/((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN))^(1/2)
  text(332, 220, paste0("MCC  = ",round(MCC,4)),
       cex=1.7, font=2)

  # # add in the specifics
  # plot(c(50, 0), c(50, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n')
  # text(5, 45, names(cm$byClass[1]), cex=1.2, font=2)
  # text(5, 40, round(as.numeric(cm$byClass[1]), 3), cex=1.2)
  # text(10, 45, names(cm$byClass[2]), cex=1.2, font=2)
  # text(10, 40, round(as.numeric(cm$byClass[2]), 3), cex=1.2)
  # text(15, 45, names(cm$byClass[5]), cex=1.2, font=2)
  # text(15, 40, round(as.numeric(cm$byClass[5]), 3), cex=1.2)
  # text(20, 45, names(cm$byClass[6]), cex=1.2, font=2)
  # text(20, 40, round(as.numeric(cm$byClass[6]), 3), cex=1.2)
  # text(25, 45, names(cm$byClass[7]), cex=1.2, font=2)
  # text(25, 40, round(as.numeric(cm$byClass[7]), 3), cex=1.2)
  #
  # # add in the accuracy information
  # text(30, 45, names(cm$overall[1]), cex=1.5, font=2)
  # text(30, 40, round(as.numeric(cm$overall[1]), 3), cex=1.4)
  # text(35, 45, names(cm$overall[2]), cex=1.5, font=2)
  # text(35, 40, round(as.numeric(cm$overall[2]), 3), cex=1.4)

}
