##### Calculate Accuracy(ACC) and Misclassification rate (Error Rate, ER) #####

AccEr_DiscMult <- function(Simu_DisMult.df, Simu_Anno.df) {

  # Simu_DisMult.df$Correctness1 <- ""

  Simu_DisMult_Res.df <- data.frame(matrix("",0,(ncol(Simu_DisMult.df)-1)))
  colnames(Simu_DisMult_Res.df) <- colnames(Simu_DisMult.df)[2:ncol(Simu_DisMult.df)]

  # MissRate: Misclassification rate
  cm_DisMult.lt <- list()
  for (j in 1:(ncol(Simu_DisMult.df)-1)) {

    for (i in 1:nrow(Simu_DisMult.df)) {
      if(Simu_DisMult.df[i,1] == Simu_DisMult.df[i,j+1]){
        Simu_DisMult_Res.df[i,j] = 0
      }else{
        Simu_DisMult_Res.df[i,j] = 1
      }
    }
    cm_DisMult.lt[j] <- sum(Simu_DisMult_Res.df[,j] == 1)/nrow(Simu_DisMult_Res.df)
    names(cm_DisMult.lt)[j] <- colnames(Simu_DisMult_Res.df)[j]
  }

  rm(i,j,Simu_DisMult_Res.df)
  Results_DisMult.df <- data.frame(TestID = colnames(Simu_DisMult.df)[2:ncol(Simu_DisMult.df)],
                                   Misclass = unlist(cm_DisMult.lt),
                                   Accuracy = 1-unlist(cm_DisMult.lt))
  ## Add annotation dataframe
  Sum_DisMult.df <- left_join(Results_DisMult.df, Simu_Anno.df)

  return(Sum_DisMult.df)
}
