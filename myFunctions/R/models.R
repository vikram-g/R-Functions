#' 
#' Univariate split for rules 
#' 
#' This function is used to identify univariate rules that provides maximum separation between goods and bads in the training data. The function returns list of variables, the best cut points in them that satisfies the constraints. The 2 constraints are - Minimum % of rows needed and minimum % of target needed - which ensures that there are certain minimum percentage of rows present when the data is split and also a certain threshold for target rate. Register a backend to automatically perform operations in parallel. 
#' 
#' @param df	Data frame with all the variables and target needed for splitting
#' @param target	Name of the target variable
#' @param min_percent_rows_needed	What is the minumum percentage of rows needed to make a split. This is after removing the missing rows
#' @param min_target_rate_needed	Minimum acceptable target rate to be present in the split to be listed in the final result
#' @return A dataframe with following columns for splits that meets the constraints         \cr
#' 1. feature - Name of the feature    \cr
#' 2. value - Value at which to make the split   \cr 
#' 3. percent_cvrd - Percentage of rows that can be rejected due to this split    \cr
#' 4. bad_rate - Bad rate of the rejected population by this split \cr
#' @export 
#' 

# Percent rcvd does not include missing values so might not be 100%
univariate_best_split <- function(df,target,min_percent_rows_needed,min_target_rate_needed,selFeats = NULL){
  require(foreach)
  if(length(selFeats) == 0){selFeats <- setdiff(names(df),target)}
  
  allSplits <- foreach(i = 1:length(selFeats), .combine = rbind) %dopar% cummulative_target_rate(df[,selFeats[i]],df[,target],selFeats[i])
  
  # Selecting rows with > 5% record included
  xx <- allSplits[allSplits$percent_cvrd > min_percent_rows_needed,]
  
  # Features that have >20% fpd at some point with at least 5% of data
  topFeats <- unique(xx$feature[xx$bad_rate > min_target_rate_needed])
  allSplits_feat <- allSplits[allSplits$percent_cvrd > min_percent_rows_needed & allSplits$feature %in% topFeats,]
  ret <- foreach(topFeat = 1:length(topFeats), .combine = rbind) %do%
  {
    allSplits_feat_new <- allSplits_feat[allSplits_feat$feature %in% topFeats[topFeat],]
    return(allSplits_feat_new[which.max(allSplits_feat_new$bad_rate),])
  }
  return(ret)
  
}

cummulative_target_rate <- function(feat, target, featName){
  df_all <- data.frame(cbind(feat, target))
  # Ascending
  track_asc <- data.frame(feature = character(),value = integer(),percent_cvrd = integer(),bad_rate = integer(), stringsAsFactors=FALSE)
  val_space <- sort(unique(df_all[,'feat']), decreasing = F, na.last = NA)
  i <- 0
  num_row <- nrow(df_all)
  for(val in val_space){
    i <- i + 1
    track_asc[i,] <- c(featName, val, sum(df_all[,'feat'] <= val, na.rm = T)/num_row, mean(df_all[df_all[,'feat'] <= val & !is.na(df_all[,'feat']),'target'], na.rm = T))
  }
  track_asc[,2:4] <- sapply(track_asc[2:4], as.numeric)
  
  # Descending
  track_desc <- data.frame(feature = character(), value = integer(),percent_cvrd = integer(),bad_rate = integer(), stringsAsFactors=FALSE)
  val_space <- sort(unique(df_all[,'feat']), decreasing = T, na.last = NA)
  i <- 0
  num_row <- nrow(df_all)
  for(val in val_space){
    i <- i + 1
    track_desc[i,] <- c(featName, val, sum(df_all[,'feat'] >= val, na.rm = T)/num_row, mean(df_all[df_all[,'feat'] >= val & !is.na(df_all[,'feat']),'target'], na.rm = T))
  }
  track_desc[,2:4] <- sapply(track_desc[2:4], as.numeric)
  
  ifelse(max(track_asc$bad_rate, na.rm = T) > max(track_desc$bad_rate, na.rm = T),return(track_asc), return(track_desc))
}