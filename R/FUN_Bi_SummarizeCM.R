#' A function for summarizing the results from multiple binary confusion matrix to dataframe
#'
#' This function allows you to summarize the results from multiple binary confusion matrix to dataframe.
#' @param cm.lt A list of multiple binary confusion matrix.
#' @param Anno.df A dataframe of the annotation table.
#' @keywords summarize the confusion matrix
#' @export
#' @examples
#' Summarize_BiCM(cm.lt, Anno.df)
#'


Summarize_BiCM <- function(cm.lt, Anno.df) {

  ## Summary of result dataframe
  # CM_Summary.df <- ""
  for (i in 1:length(cm.lt)) {

    if(i==1){
      CM_Summary.df <- data.frame(c(cm.lt[[i]][["overall"]], cm.lt[[i]][["byClass"]]))
      colnames(CM_Summary.df)[i] <- names(cm.lt[i])
      CM_Summary.df <- CM_Summary.df %>% t() %>% as.data.frame()

    }else{
      Results_S.df <- data.frame( c(cm.lt[[i]][["overall"]], cm.lt[[i]][["byClass"]]))
      colnames(Results_S.df) <- names(cm.lt[i])
      Results_S.df <- Results_S.df %>% t() %>% as.data.frame()
      CM_Summary.df <- rbind(CM_Summary.df,Results_S.df)
    }
  }

  rm(i,Results_S.df)

  CM_Summary.df <- data.frame(TestID=row.names(CM_Summary.df),CM_Summary.df)
  CM_Summary.df <- left_join(CM_Summary.df, Anno.df)

  return(CM_Summary.df)
}
