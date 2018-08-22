
########################################################################################################################################################
#' 
#' Stratified sampling 
#' 
#' Splits the data into any number of samples, holding the distribution of any number of variables (passed as parameter "strata") constant across splits. Pass the target variable to maintain the target rate same across all the splits
#' 
#' @param strata	Data frame containing variables or a single variable that should be used for stratification. Include the target/response/model dependent variable to maintain constant target rate across samples
#' @param split_ratio	A list with length equaling number of samples required, containing proportions of rows needed in each sample. If adds to < 1 then another sample for the balance value is added. If adds > 1 then only they are normalized to 1
#' @param seed	Seed to set. Default is 1
#' @return A list of numbers with length equal to number of rows of data provided. nth value in the list corresponds to the sample nth row of the data belongs to.
#' @export 
#' 

stratified_sample <- function(strata, split_ratio, seed = 1){
  # If sum of split_ratio < 1 then creating another one so that sum is 1
  if(sum(split_ratio) < 1){split_ratio <- c(split_ratio, 1- sum(split_ratio))}
  # Standardising so that sum would be 1
  split_ratio <- split_ratio/sum(split_ratio)
  
  # If strata is single columns converting to data frame by adding new columns of unique value
  if (class(strata) == "data.frame"){strata$tmp_uid <- seq(1,nrow(strata))}
  if (class(strata) != "data.frame"){strata <- data.frame(strata = strata, tmp_uid = seq(1,length(strata)),stringsAsFactors = F)}
  
  # Converting any factors to characters - it is just easy to handle them as character
  strata[,names(strata)[sapply(strata, class) == "factor"]] <- as.character(strata[,names(strata)[sapply(strata, class) == "factor"]])
  strata <- replace_all_NAs(strata,"Missing")
  # Splitting the data set by features provided in strata 
  sp <- split(strata$tmp_uid, strata[,!names(strata) %in% "tmp_uid"])
  set.seed(seed)
  # For each split, samples (numbers 1,2,3... till length(split_ratio)) are created according to split_ratio. Then they are shuffled and combined into list
  split_samples <- do.call(c,lapply(sp, function(x) sample(sapply((1:length(x)) /length(x) , function (xx) sum(cumsum(split_ratio) < xx) + 1),length(x),replace = F) ))
  # Samples are sorted according to original order in the data and returned
  split_samples[unname(as.numeric(unlist(sp)))] <- split_samples
  return(split_samples)
}


########################################################################################################################################################
#'
#' Sub sampling with fixed target
#' 
#' Down samples the majority class (assumed to take value 0; with minor = 1) while retaining all minor samples, to the target_ratio provided, stratified by multiple columns. \cr
#' The target_ratio provided should be higher than actual ratio in the unsampled data. If this is not true, no sampling for that strata is done and expected target ratio can not be maintained.
#' 
#' @param df	data framet that needs to be sampled
#' @param target_ratio	Integer 0-1 of the expected target ratio in the sampled data. This should be more than current target ratio or else all records will be included for current strata and target ratio can not be maintained
#' @param targetCol	Target column (response, fpd, chargeoff etc..)
#' @param strata	list of column names by which the data needs to be stratified
#' @return List of row numbers that needs to be retained in the current data set
#' @export 
#' 

down_sample_stratified <- function(df, target_ratio, targetCol,strata){
  require(dplyr,quietly = T)
  if(target_ratio < 1){target_ratio <- target_ratio * 100}
  if(sum(!unique(df[,targetCol]) %in% c(1,0,NA)) > 0){stop("Target Variable has values other than 0,1 & NA")}
  df$rowid <- seq(1,nrow(df),1)
  df <- df[,c("rowid",targetCol,strata)]
  df <- df[!is.na(df[,targetCol]),]
  
  # Handling data types and missing values in strata
  # Converting all strata to character
  if(length(strata) == 1) {
    df[,strata] <- as.character(df[,strata])
    df[is.na(df[,strata]),strata] <- "Missing"
    }
  else if(length(strata) > 1) {
    df[,strata] %>% mutate_all(as.character) -> df[,strata]
    df[,strata] <- replace_all_NAs(df[,strata],replace_with = "Missing")
  }
  
  # Getting the required number of rows from each strata
  strata_ratio_count <- df %>% filter_(paste0(targetCol," == 1")) %>% group_by_(.dots = lapply(eval(strata), as.symbol)) %>% summarize(count = n(), count_adj = round((count/target_ratio)*(100 -target_ratio)))
  df_sample <- df[df[,targetCol] == 1,]
  
  for (i in 1:nrow(strata_ratio_count)){
    set.seed(1)
    if(length(strata) > 1){
      df_sample <- tryCatch(rbind(df_sample,
                                  df[rowSums(df[,strata] == c(strata_ratio_count[i,1:length(strata)])) == length(strata),] %>% filter_(paste0(targetCol," == 0")) %>% sample_n(size = strata_ratio_count$count_adj[i], replace = F)),
                            error = function(cond){
                              message(paste0("Not enough sample, hence taking all 0s in following strata - ",paste(c(strata_ratio_count[i,1:length(strata)]), collapse = ",")))
                              rbind(df_sample,
                                    df[rowSums(df[,strata] == c(strata_ratio_count[i,1:length(strata)])) == length(strata),] %>% filter_(paste0(targetCol," == 0"))) })
    }
    if(length(strata) == 1){
      df_sample <- tryCatch(rbind(df_sample,
                                  df[(df[,strata] == c(strata_ratio_count[i,1:length(strata)])) == length(strata),] %>% filter_(paste0(targetCol," == 0")) %>% sample_n(size = strata_ratio_count$count_adj[i], replace = F)),
                            error = function(cond){
                              message(paste0("Not enough sample, hence taking all 0s in following strata - ",paste(c(strata_ratio_count[i,1:length(strata)]), collapse = ",")))
                              rbind(df_sample,
                                    df[(df[,strata] == c(strata_ratio_count[i,1:length(strata)])) == length(strata),] %>% filter_(paste0(targetCol," == 0"))) })
      
    }
    
  }
  return(df_sample$rowid)
}


########################################################################################################################################################

#'
#' Delimiter present in the data?
#' 
#' When writing data frame to delimited flat file, the data should be checked for commonly used delimitor presence in the data. For instance if a column has user entered address which might have "," in them, then comma should be avoided as a delimitor to write the file. This function takes a data frame and returns list of variable names if any that have commonly used delimitors in them. 
#' @param df	R data frame that needs to be checked for presence of delimitor
#' @return List of column names (if any) that have common delimitors. 
#' @export 
#' 
col_with_delimiter <- function(df){
  # Takes a data frame and returns list of columns which has potential delimiter
  # This is to test the data before writing the file in specified format
  return(names(df)[which (sapply(df, function(x) sum(grepl("[,|?;]", x)) > 0))])
}


########################################################################################################################################################

#'
#'Missing value handling
#'
#' Imputes the missing values either using median or WoE. Can also impute the missing value across train 
#' 
#' @param df	Data frame with only the columns to be imputed and target value(if WoE method is used)
#' @param method	String value of method used to impute the missing values "median" or "WoE".
#' @param acrossTrainTest	Logical value. If TRUE, then median only from train data is used to impute both train and test. 
#' @param trainIndicator	List of logical values indicating which row belongs to train. Length should be equal to number of rows of dataframe 
#' @param WoE_bins	Numeric value. If method is "WoE" this parameter is used to indicate number of bins used
#' @param WoE_target	Name of target variable when method is "WoE"
#' @return The original data frame with all the missing values imputed  
#' @export 
#' 


missing_impute <- function(df, method = "median",  acrossTrainTest = F, trainIndicator = NULL, WoE_bins = 10, WoE_target = NULL){
  # Possible methods available
  # median - replace all missing with median of the feature
  # WoE - WoE bin imputation; Also need to provide the parameter WoE_bins. Should also provide the target variable in the data set
  
  # acrossTrainTest
  # If acrossTrainTest is TRUE then only train data is used to identify median or WoE and test data is imputed with that value
  message("Special values need to be handled before imputing missing values, assuming this is done")
  if(!acrossTrainTest){
    if(method == "median"){
      # Get median 
      medians <- sapply(df, function(x) median(x, na.rm = T))
      nm <- names(df)
      df <- sapply(1:ncol(df), function(x) {ifelse(is.na(df[,x]),medians[x],df[,x])})
      names(df) <- nm
    }
    if(method == "WoE"){
      require(Information)
      iv_tables <- create_infotables(data = df,  y = WoE_target, bins = WoE_bins,
                                     trt = NULL, ncore = 10, parallel = F)
      colToImpute <- names(df)[colSums(is.na(df)) > 0]
      colToImpute <- intersect (names(iv_tables$Tables),colToImpute ) # Some time IV table not created if there aren't enough levels
      
      for( col in colToImpute){
        
        iv_col <- iv_tables$Tables[[col]]
        closest_bin <- which.min(abs(iv_col$WOE[iv_col[,1] != "NA"] - iv_col$WOE[iv_col[,1] == "NA"])) + 1
        if (closest_bin == 2){
          df[is.na(df[,col]),col] <- as.numeric(unlist(strsplit(substr(iv_col[closest_bin,1],2,nchar(iv_col[closest_bin,1])-1),",")))[1]
        }
        else if (closest_bin == nrow(iv_col)){
          df[is.na(df[,col]),col] <- as.numeric(unlist(strsplit(substr(iv_col[closest_bin,1],2,nchar(iv_col[closest_bin,1])-1),",")))[2]
        }
        else {
          df[is.na(df[,col]),col] <- mean(as.numeric(unlist(strsplit(substr(iv_col[closest_bin,1],2,nchar(iv_col[closest_bin,1])-1),","))))
        }
      }
      df[,WoE_target] <- NULL
    }
    return (df)
  }
  
  if(acrossTrainTest){
    if(method == "median"){
      # Get median 
      medians <- sapply(df[trainIndicator,], function(x) median(x, na.rm = T))
      nm <- names(df)
      df <- sapply(1:ncol(df), function(x) {ifelse(is.na(df[,x]),medians[x],df[,x])})
      names(df) <- nm
    }
    if(method == "WoE"){
      require(Information)
      iv_tables <- create_infotables(data = df[trainIndicator,],  y = WoE_target, bins = WoE_bins,
                                     trt = NULL, ncore = 40, parallel = F)
      colToImpute <- names(df)[colSums(is.na(df[trainIndicator,])) > 0]
      colToImpute <- intersect (names(iv_tables$Tables),colToImpute ) # Some time IV table not created if there aren't enough levels
      
      for( col in colToImpute){
        
        iv_col <- iv_tables$Tables[[col]]
        closest_bin <- which.min(abs(iv_col$WOE[iv_col[,1] != "NA"] - iv_col$WOE[iv_col[,1] == "NA"])) + 1
        if (closest_bin == 2){
          df[is.na(df[,col]),col] <- as.numeric(unlist(strsplit(substr(iv_col[closest_bin,1],2,nchar(iv_col[closest_bin,1])-1),",")))[1]
        }
        else if (closest_bin == nrow(iv_col)){
          df[is.na(df[,col]),col] <- as.numeric(unlist(strsplit(substr(iv_col[closest_bin,1],2,nchar(iv_col[closest_bin,1])-1),",")))[2]
        }
        else {
          df[is.na(df[,col]),col] <- mean(as.numeric(unlist(strsplit(substr(iv_col[closest_bin,1],2,nchar(iv_col[closest_bin,1])-1),","))))
        }
      }
      df[,WoE_target] <- NULL
    }
    return (df)
  }
  
}

########################################################################################################################################################
# Replace all empty strings with missing values
#' 
#' Replace empty string with NA 
#' 
#' Replaces the empty string values in the character variables of data frame with missing values.  
#' 
#' @param df	Data frame with all the columns for which missing values in character columns need to be replaced with NAs
#' @return The data frame passed to the function with all empty strings in character columns replaced by NAs
#' @export 
#'   
replace_empty_string_with_NA <- function(df){
  char_names <- names(df)[sapply(df,class) == "character"]
  df[,char_names] <- apply(df[,char_names], 2, function(x) gsub("^$|^ $", NA, x))
  return(df)
  }


########################################################################################################################################################


# Handling binary variables that are present as character
#' Capping and flooring
#' 
#' Does capping and if required flooring one variable at a time. Any values outside 1.5 times inter quartile range is capped and floored with values at 95th and 5th percentile respectively. Can also be done across folds (i.e use training data to identify interquartile range and values to replace and apply them on both train and test). If standard deviation becomes 0 after cap and floor, which can happen with variables that have very few unique values, the original variable is returned without transformation. 
#' 
#' @param x	List of numerical values which needs to be capped and floored
#' @param train_ind	Logical list with length equal to x, indicating the values of x that belong to train. When provided, transformations are done across samples - i.e training data to identify interquartile range and values to replace and apply them on both train and test. Defaults to NULL which will use the entire distribution to perform transformation. 
#' @param floor	Logical indicator for should flooring also be done. Defaults to TRUE
#' @return A list with length same as x containing transformed values 
#' @export 
#' 

cap_floor <- function(x, train_ind = NULL, floor = T){
  # x is the column to be capped & floored
  # train_ind is list containing the TRUE for the train/featsel data 
  if(length(train_ind) == 0){ train_ind <- rep(TRUE,length(x))}
  orig_x <- x
  qnt <- quantile(x[train_ind], probs=c(.25, .75), na.rm = T)
  caps <- quantile(x[train_ind], probs=c(.05, .95), na.rm = T)
  H <- 1.5 * IQR(x[train_ind], na.rm = T)
  x[x < (qnt[1] - H)] <- caps[1]
  if(floor) {x[x > (qnt[2] + H)] <- caps[2]}
  (ifelse(sd(x[train_ind], na.rm = T) == 0,return(orig_x),return(x)))
}


########################################################################################################################################################

#' 
#' Handle binary variables present as character 
#' 
#' Bureau variables on occasions has binary variables encoded as true/false, y/n, green,blue. This function turns all those variables into 1s & 0s. \cr  
#' colsToLeave parameter can be used to leave any variables that you might want to not replace
#' 
#' @param df	The working data frame, character variables would be filtered inside the function
#' @param colsToLeave	Optional parameter to provide list of variables that needs to be left alone
#' @return Original data frame with all binary characters replaced by 1/0s
#' @export 
#' 


handleCharBinaries <- function(df,colsToLeave = NULL)
{
  charCols <- names(df)[sapply(df,class) == 'character']
  if(length(colsToLeave) > 0) {charCols <- setdiff(charCols, colsToLeave)}
  
  # True/False
  TrueFalse_cols <- sapply(df[,charCols],
                           function(eachCol)sum(tolower(unique(eachCol)) %in% c('true','false',NA,"")) == length(unique(eachCol))) 
  TrueFalse_cols <- charCols[TrueFalse_cols] # Getting only features with Ttrue/false values
  df[,TrueFalse_cols] <- sapply(df[,TrueFalse_cols],
                                function(eachCol)as.numeric(ifelse(tolower(eachCol) == 'true',1, ifelse(tolower(eachCol) == 'false',0,NA))))
  
  # Yes/No
  TrueFalse_cols <- sapply(df[,charCols],
                           function(eachCol)sum(tolower(unique(eachCol)) %in% c('y','n',NA,'')) == length(unique(eachCol))) 
  TrueFalse_cols <- charCols[TrueFalse_cols] # Getting only features with True/False values
  df[,TrueFalse_cols] <- sapply(df[,TrueFalse_cols],
                                function(eachCol)as.numeric(ifelse(tolower(eachCol) == 'y',1, ifelse(tolower(eachCol) == 'n',0,NA))))
  
  # Green/Red
  TrueFalse_cols <- sapply(df[,charCols],
                           function(eachCol)sum(tolower(unique(eachCol)) %in% c('green','red',NA,'')) == length(unique(eachCol))) 
  TrueFalse_cols <- charCols[TrueFalse_cols] # Getting only features with green/red values
  df[,TrueFalse_cols] <- sapply(df[,TrueFalse_cols],
                                function(eachCol)as.numeric(ifelse(tolower(eachCol) == 'green',1, ifelse(tolower(eachCol) == 'red',0,NA))))
  return(df)
}



########################################################################################################################################################


# Creating dummy for char vars
#' One hot encoding (dummy var creation)
#' 
#' Takes a dataframe and creates one hot encoding (dummy variables) for all character variables in it. Function creates one variable for each level of character variable. The new names will be of the patter "OriginalVariableName_level". Missing values get their own one hot encodes with "_missing" appeneded to variable name.   
#' 
#' @param df	The master data frame. It can also contain variables of other types which would be ignored
#' @param level_cutoff	Any variable that has more than this number of levels are left untouched. This is to avoid the data set size blowing up. Defaults to 1000
#' @param colsToLeave	List of column names that needs to left alone. Defaults to NULL
#' @return Returns data frame with OHEs created for character variables and the original variables removed
#' @export 
#'  

one_hot_encode <- function(df, level_cutoff = 1000,colsToLeave = NULL){
  # Filtering for character variables only
  charVars <- names(df)[sapply(df,class) == 'character']
  # Removing colsToLeave variables
  if(length(charVars) > 0) {charVars <- setdiff(charVars, colsToLeave)}
  # Removing variables that has more than levels more than the cut off parameter
  charVars <- charVars[sapply(charVars, function(x) length(unique(df[,x])) <= level_cutoff )]
  # Replace any space to missing value
  df[,charVars] <- apply(df[,charVars], 2, function(x) gsub("^$|^ $", NA, x))
  # Replace NAs with "Missing" so that it would be prefixed to column name for missing rows
  df[,charVars] <- replace_all_NAs(df[,charVars], replace_with = "Missing")
  # Adding "_" to column names so that it would appear in the final variable name as variableName_level
  names(df)[names(df) %in% charVars] <- paste0(charVars, "_")
  charVars <- paste0(charVars, "_")
  # Creating OHEs and appending them
  for (col in charVars){
    df <- cbind(df,model.matrix(as.formula(paste("~ ",col," -1")),data = df))
  }
  # Removing features for which OHEs created
  df[,charVars] <- NULL
  return(df)
}


########################################################################################################################################################

#'
#'Unsupervised feature reduction
#'
#'Performs unsupervised feature reduction on the data set based on following criteria   \cr        
#' 1. Variables with 0 variance are removed by default    \cr 
#' 2. Variables with only 0s and NAs are removed by default   \cr  
#' 3. Variables with low percentage of non missing or zero rows if nonMissingRate parameter is set \cr 
#' 4. Variables with low variance (percentage of non most frequence values) if varianceCutOff parameter is set \cr
#' 
#' @param df	Master data frame with all columns
#' @param excludeCols	List of columns to be excluded from feature reduction. This contains any IDs, target variable or other non independent variable for the model
#' @param nonMissingRate	Remove any variables with \% of non missing rows, non empty or non zero values less than this   
#' @param varianceCutOff	Removes variables that do not have much variance with just one value representing large \% of records. Any variables that has (1 - \% of most frequent variable in non missing rows) less than varianceCutOff is removed       
#' @param returnDropReasons	Logical value. If TRUE a dataframe of variables removed along with reason removed returned along with the reduced data set       
#' @return If returnDropReasons is set to FALSE, the reduced data frame after features removed would be returned. All excludeCols are also added to the final data frame returned    \cr
#' If returnDropReasons is set to TRUE, the reduced data frame and another data frame with list of removed variables with reasons removed is returned. All excludeCols are also added to the final data frame returned  
#' @export 
#' 


unsupervised_feat_reduction <- function(df,excludeCols = NULL, nonMissingRate = NULL, varianceCutOff = NULL , returnDropReasons = F){
  # Data frame to hold drop reasons
  drp_rsn <- data.frame(feature = character(), drop_reason = character(),stringsAsFactors = F)
  if(length(excludeCols) > 0) 
  {
    df_excludeCols <- df[,excludeCols]
    df <- df[,setdiff(names(df),excludeCols)]
  }
  
  # Drop 0 variance features
  allSame <- sapply(df,function(x)length(unique(x)) == 1)
  
  if(length(allSame) > 0){
  allSame <- names(allSame[allSame])
  drp_rsn <- rbind(drp_rsn, data.frame(feature = allSame,drop_reason = 'all_missing'))
  }
  df[,allSame] <- NULL
  
  # Columns with just 0s and NAs
  zerosAndNAs <- sapply(df,function(x){length(unique(x)) == 2 & sum(is.na(x)) > 0 & 0 %in% unique(x)})
  
  if(length(zerosAndNAs) > 0){
  zerosAndNAs <- names(zerosAndNAs[zerosAndNAs])
  drp_rsn <- rbind(drp_rsn, data.frame(feature = zerosAndNAs,drop_reason = 'only_0_NAs'))}
  df[,zerosAndNAs] <- NULL
  
  # Remove features that has less than X% of non-missing or zero values
  if(length(nonMissingRate) > 0)
  {
  message("Inside loop")
    cutOff <- nonMissingRate
    percNonNaZero <- sapply(df, function(x)(sum(!is.na(x)) - sum(x %in% c(0,""," "), na.rm = T))/nrow(df))
    percNonNaZero_less2P <- names(percNonNaZero)[as.numeric(percNonNaZero) < cutOff]
    if(length(percNonNaZero_less2P) > 0){drp_rsn <- rbind(drp_rsn, data.frame(feature = percNonNaZero_less2P,drop_reason = paste0('Missing_',nonMissingRate,"%")))}
    df[,percNonNaZero_less2P] <- NULL
  }
  
  # Remove low variance features
  if(length(varianceCutOff) > 0)
  {
    cutOff <- varianceCutOff
    percNonNaZero <- sapply(df, function(x)max(table(x)/sum(!is.na(x))))
    percNonNaZero_less2P <- names(percNonNaZero)[as.numeric(percNonNaZero) > (1-cutOff)]
    if(length(percNonNaZero_less2P) > 0){drp_rsn <- rbind(drp_rsn, data.frame(feature = percNonNaZero_less2P,drop_reason = paste0('SameValue_',varianceCutOff,"%")))}
    df[,percNonNaZero_less2P] <- NULL
  }
  
  if(length(excludeCols) > 0) 
  {
    df <- cbind(df, df_excludeCols)
  }
  if(returnDropReasons){ return (list("df" = df,"drp_rsn" = drp_rsn))}
  else{ return (df)}
}


########################################################################################################################################################

#'
#' Correlation based feature reduction
#' 
#' Finds highly correlated features and returns list of variables to remove. The algorithm generally picks the variable with higher missing rate to be removed from a pair. \cr
#' There are few of difficulties in correlation based feature reduction -  \cr  
#' 1. \strong{Computational intensity} - With huge data as ours, it takes a long time to compute correlations of all pairs. Hence by setting the parameter "nSubsequent" for each column, we could look at only next "N" columns for comparison. With very high correlation cutoff such as 99\%, just looking at next 10 features generally covers most of the correlated variables as they tend to bunch up together. If cut off is lowered, the nSubsequent should be increased to be more effective.       \cr
#' 2. \strong{Chain drops} - Consider scenario where variable A and B have 95\% correlation and A gets dropped, then B and C have 95\% correlation and B gets dropped too and if A and C are not that highly correlated then we might have lost information that variable A provided. By setting chainDrop as FALSE , this scenario is overcome by not dropping C instead of B while comparing B & C in above case          \cr
#' 3. \strong{Low information overlap} - It could be possible to have very low overlap of non-missing values between variable pairs and it would be unfair to drop one of them based on correlation found in those few overlapping rows. Hence by default variable pairs are considered only if the # of rows with both variables non-missing/ # of rows with at least one variable is non-missing is >80\%  \cr
#' 
#' @param df	Master data frame with all columns. Function will filter for numeric and integer columns
#' @param nSubsequent	Number of subsequent columns with which each column should be compared in order to reduce computation time. Defaults to 10, which seems reasonable for >0.98 cut offs
#' @param corCutoff	Cut off value for correlation. Any pair with correlation above this will have one of it's value dropped. Defaults to 0.98
#' @param chainDrop	Prevents a chain of variable drops reducing the information provided by variables more than intented. Look the description above for more details on this. Defaults to FALSE
#' @param returnNamesOnly	Logical. If TRUE, returns only names to be removed. If FALSE removes data frame with all pairs meeting cutoff and suggestion on which column to remove
#' @return If returnNamesOnly is TRUE, returns list of variable names to be removed     \cr 
#' If returnNamesOnly is FALSE, returns a dataframe with following columns   \cr  
#' 1. V1 - Variable 1 of the pair    \cr
#' 2. V2 - Variable 2 of the pair  \cr  
#' 3. Correlation - Pairwise correlation between the 2 above variables \cr
#' 4. missingMatchRate - # of rows with both variables non-missing/ # of rows with at least one variable is non-missing (internal value used)   \cr 
#' 5. ord - Rank order sorted by correlation     \cr
#' 6. rem - Indicator on which varaible to remove; 1 = remove variable 1, 2 = remove variable 2, 0 = do not remove either of them \cr
#' @export 
#' 

correlated_feats <-  function(df,nSubsequent = 10, corCutoff = 0.98,chainDrop = F, returnNamesOnly = T){
  # Filtering only numeric and integer columns
  df <- df[, names(df)[sapply(df,class) %in% c("numeric","integer")]]
  df_cor <- data.frame(V1 = '', V2 = '', Correlation = 0, missingMatchRate = 0, stringsAsFactors = F)
  i <- 0
  nm <- names(df)
  for (col in 1:(ncol(df)-1)){
    lst <- min((col+nSubsequent),ncol(df))
    for (col2 in (col+1):lst){
      i <- i+1
      df_cor[i,1] <- nm[col]
      df_cor[i,2] <- nm[col2]
      df_cor[i,3] <- cor(df[,col], df[,col2],use = 'pairwise.complete.obs')
      missPattern <- paste(is.na(df[,col])*1, is.na(df[,col2])*1, sep = "")
      df_cor[i,4] <- sum(missPattern == '11')/sum(missPattern != '00')
    }
  }
  
  df_cor <- df_cor[df_cor$Correlation > corCutoff & !is.na(df_cor$Correlation) & df_cor$missingMatchRate > 0.8 & !is.na(df_cor$missingMatchRate),]
  df_cor$ord <- rank(-df_cor$Correlation, ties.method = 'min')
  df_cor <- df_cor[order(df_cor$Correlation, decreasing = T),]
  df_cor$rem <- 0 # flag to indicate which column was removed
  df_cor[1,'rem'] <- 2
  
  for (i in 2:nrow(df_cor)){
    # Checking if any of the 2 variable has already been dropped, if yes then drop the same variable in this case too
    df_cor$rem[i] <- ifelse(df_cor$V2[i] %in% c(df_cor$V2[df_cor$rem == 2],df_cor$V1[df_cor$rem == 1]),2,ifelse(df_cor$V1[i] %in% c(df_cor$V2[df_cor$rem == 2],df_cor$V1[df_cor$rem == 1]),1,0))
    if(df_cor$rem[i] != 0){ next}
    # If both the variables haven't been dropped earlier 
    # Check if the variable under consideration had already a pair that got dropped
    if(df_cor$rem[i] == 0){
      V1_has_pair_dropped <- df_cor$V1[i] %in% c(df_cor$V2[df_cor$rem == 1],df_cor$V1[df_cor$rem == 2])
      V2_has_pair_dropped <- df_cor$V2[i] %in% c(df_cor$V2[df_cor$rem == 1],df_cor$V1[df_cor$rem == 2])
      # If V2 had dropped earlier pair and V1 doesn't then drop V1
      if(!(V1_has_pair_dropped) & (V2_has_pair_dropped)) {
        df_cor$rem[i] <- 1
        next
      }
      # If V1 had dropped earlier pair and V2 doesn't then drop V1
      if((V1_has_pair_dropped) & !(V2_has_pair_dropped)) {
        df_cor$rem[i] <- 2
        next
      }
    }
    # If both of them did not have dropped pair (or) if chainDrop rule is set, then drop the one with most missing values
    if((!(V1_has_pair_dropped) & !(V2_has_pair_dropped)) | chainDrop) {
      v1_nonmiss <- sum(!is.na(df[,df_cor$V1[i]]))
      v2_nonmiss <- sum(!is.na(df[,df_cor$V2[i]]))
      df_cor$rem[i] <- ifelse(v1_nonmiss >= v2_nonmiss,2,ifelse(v1_nonmiss < v2_nonmiss,1,0))
      next
    }
  }
  ifelse(returnNamesOnly, return(unique(c(df_cor$V2[df_cor$rem == 2],df_cor$V1[df_cor$rem == 1]))),return(df_cor))
}


########################################################################################################################################################

#' 
#' Remove table name prefix
#' 
#' Often when pulling data from relational database, the table name gets prefixed to the column names. This function removes such prefix if any (if there are no prefix, this function doesn't affect the column names) 
#' 
#' @param x	Data frame which needs table name prefix to be removed
#' @return The original data frame with prefix stripped off from the names 
#' @export 
#' 

rmv_tbl_name_prefix <- function(x){ 
  names(x) <- sub(".*\\.", "", names(x))
  return(x)
}

########################################################################################################################################################

# Replaces all missing values in the data frame with provided value - this df[is.na(df)] <- -999 fails where there are too many rows
#' Replace all NAs in data frame
#' 
#' Replaces all missing values in the data frame with provided value - this df[is.na(df)] <- -999 fails where there are too many rows
#' 
#' @param df	Dataframe with all the columns for which missing values need to be replaced
#' @param replace_with	Value that the missing values should be replaced with. Defaults to -999
#' @return Returns the original data frame with all NAs replaced with the provided value
#' @export 
#' 

replace_all_NAs <- function(df,replace_with = -999){
  require(dplyr,quietly = TRUE)
  return (df %>% mutate_all(funs(replace(., is.na(.), replace_with))))
}


########################################################################################################################################################