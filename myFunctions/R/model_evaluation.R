# KS score
#' Max KS score  
#' 
#' Function returns the maximum KS score. If top_N_percent parameter is supplied then returns the max KS within the top n percentile of the predictions. 
#' 
#' @param actual	List of actual values - 0s and 1s of the target variable
#' @param prediction	List of predicted probabilities
#' @param top_N_percent	Optional parameter. Should have values between 0 and 1. When provided, returns the max ks within the top n percent of the predicted probability.
#' @return Max KS score. 
#' @export 
#' 

max_ks_score <- function(actual, prediction, top_N_percent = 1){
  k <- as.data.frame(cbind(actual, prediction))
  names(k) <- c('act','pred')
  k <- k[order(k$pred, decreasing = T),]
  ks <- (cumsum(k$act)/sum(k$act)) - (cumsum(1-k$act)/sum(1- k$act))
  ks <- ks[seq(1,round(nrow(k) * top_N_percent),1)] # Picking top N percent rows
  return(max(ks, na.rm = T))
}


# KS score by decile
#' KS score by each decile
#' 
#' Function returns the KS score at each decile of the predicted probability.
#' 
#' @param actual	List of actual values - 0s and 1s of the target variable
#' @param prediction	List of predicted probabilities
#' @return List of numeric values with KS at each deciles 
#' @export 
#' 

ks_score_by_decile <- function(actual, prediction){
  k <- as.data.frame(cbind(actual, prediction))
  names(k) <- c('act','pred')
  k <- k[order(k$pred, decreasing = T),]
  ks <- (cumsum(k$act)/sum(k$act)) - (cumsum(1-k$act)/sum(1- k$act))
  return(ks[round(seq(0.1,1,0.1) * length(ks),digits = 0)])
}

#' AUC
#' 
#' This function calculates the AUC score for the given actual and predictions. If auc needs to be calculated within each segment such as state or vintage, passing the segment to the parameter segment would return AUC by each segment. At present only one segment variable can be used at a time. 
#' 
#' @param actual	List of actual values - 0s and 1s of the target variable
#' @param prediction	List of predicted probabilities
#' @param segment	Optional parameter. One segment variable can be passed if the auc has to be calculated within each segment
#' @return If no segment is passed, returns a numberic value that is AUC   \cr  
#' If segment is passed, a data frame containing segment and correponding AUC is returned. One row of overall AUC is also returned 
#' @export 
#' 

calculate_auc <- function(actual, prediction, segment = NULL){
  if(length(segment) == 0){
    return((as.numeric(ROCR::performance(ROCR::prediction(prediction, actual),'auc')@y.values)))
  }
  if(length(segment) > 0){
    segs <- unique(segment)
    res <- data.frame(segment = character(), auc = numeric(), stringsAsFactors = F)
    res[1,] <- c("Overall",(as.numeric(ROCR::performance(ROCR::prediction(prediction, actual),'auc')@y.values)))
    i <- 2
    for (seg in segs){
      pred <- prediction[segment == seg]
      act <- actual[segment == seg]
      res[i,] <- c(seg,(as.numeric(ROCR::performance(ROCR::prediction(pred, act),'auc')@y.values)))
      i <- i+1
    }
    return(res)
  }
}

#' 
#' Gains chart data  
#' 
#' This function calculates all the data needed to create a lift and gains chart. 
#' @param actual	List of actual values - 0s and 1s of the target variable
#' @param prediction	List of predicted probabilities
#' @param groups	Number of groups the rows should be aggregated. This will be same as number of rows returned by the function. Defaults to 10.
#' @return A dataframe containing following columns \cr         
#' 1. depth -	cumulative percentage of file covered by each row of the gains table (e.g. 10,20,30,...,100).      \cr
#' 2. obs -	number of observations in each row.     \cr
#' 3. cume.obs - cumulative number of observations in each row.       \cr
#' 4. mean.resp -	mean response in each row.     \cr
#' 5. cume.mean.resp -	cumulative mean response in each row.      \cr
#' 6. cume.pct.of.total -	cumulative percent of total response.       \cr
#' 7. lift -	lift index. The lift index is 100 times the mean.resp for the row divided by the cume.mean.resp for the last row.       \cr
#' 8. cume.lift -	cumulative lift index. It is 100 times the cume.mean.resp for the row divided by the cume.mean.resp for the last row.  \cr   
#' 9. mean.prediction -	mean predicted response in each row.       \cr
#' 10. min.prediction -	minimum predicted response in each row. min.prediction and max.prediction can be used to construct decision rules for applying the model.      \cr
#' 11. max.prediction -	maximum predicted response in each row. \cr
#' @export 
#' 

gains_data <- function(actual, prediction, groups = 10){
  return(data.frame(sapply(gains::gains(actual, prediction)[1:11],cbind)))
}



# For the given train and test auc, the function measures the % drop in auc. This should ideally be < 0.1
#' AUC drop rate 
#' 
#' Calculates the AUC drop rate between train and test AUCs. Ideally we would want to keep this value less than 10% in order to avoid over fit.
#' 
#' @param train_auc	Numeric value of AUC from train data. This could CV train or overall training data auc
#' @param test_auc	Numeric value of AUC from test data. This could CV test or validation auc
#' @return Percentage of drop seen in AUC  
#' @export 
#' 

auc_drop_rate <- function(train_auc, test_auc){
  return(1 - (test_auc - 0.5)/(train_auc - 0.5))
}

