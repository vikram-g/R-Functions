
########################################################################################################################################################
#' Check if delimiter is inside any of the fields
#' 
#' This function takes flat file as input and checks if the delimiter is present anywhere inside any of the fields. 
#' This will help prevent parsing issue in cases such as when there are pipe separated values present within a column in a pipe delimited file. 
#'    Methodology - The function calculates number of columns at each row and returns the max & min values across the entire file. Any difference between max and min indicates issue. 
#'    
#' @param fileName	Complete path of the flat file to be analyzed
#' @param fieldSeperator	The field separator used in the file
#' @param nrows	First n rows to be looked into. In case of large files using this parameter could speed things up by looking at only sample of rows. Defaults to NULL which looks at all the rows
#' @return Prints whether the field separator is present in any of the fields along number of maximum and minimum number of columns identified. 
#' @export 
#' 

delimiter_inside_field <- function(fileName, fieldSeperator, nrows = NULL){
  # Tests if the delimiter used is not part of any column in the flat file
  # Takes a flat file location as parameter and prints if all columns are same length
  
  # Parsing file name into folder and file name 
  curDir <- getwd()
  split_path <- function(x) if (dirname(x)==x) x else c(basename(x),split_path(dirname(x)))
  setwd(file.path(paste(split_path(fileName)[seq(length(split_path(fileName)),2,-1)], collapse = "/")))
  fileName <- split_path(fileName)[1]
  if(length(nrows) == 0){
    colCount <- as.numeric(system(command = paste0("awk -F '",fieldSeperator,"' '{print NF}' ",fileName," | sort -nu"), intern = T))
  }
  if(length(nrows) > 0){
    colCount <- as.numeric(system(command = paste0("head -",nrows," ",fileName," | awk -F '",fieldSeperator,"' '{print NF}' | sort -nu"), intern = T))
  }
  setwd(curDir)
  if(max(colCount) == min(colCount))
  {
    message(paste0("Field separator is not in any of the field. All rows have same number of fields - ",colCount[1]))
  }
  else 
  {
    message(paste0("Field separator is in at least one of the field. Number of columns is ranging from ",max(colCount), " to ",min(colCount)))
  }
}

########################################################################################################################################################

#' Columns sum from flatfile
#' 
#' The function calculates the sum of columns from flat file. This sum can later be compared with column sum from the final data after all processing is done. If a back end is registered, the processing is automatically done in parallel. Any character values present such as column names is ignored.
#' 
#' @param fileName	Name of the flat file to be analysed with full path.
#' @param fieldSeperator	The field separator used in the file
#' @param colsToCount	List indicating column numbers for which the sums need to be calculated
#' @return A list with column sum in the order corresponding to the input flat file.  
#' @export 
#' 

column_sums_flat_file <- function(fileName, fieldSeperator,colsToCount = NULL){
  library(foreach)
  # Function returns sum of all the columns from flat file provided
  # Provide list of column numbers through parameter colsToCount to filter for columns
  # regist a back end to perform the operation in parallel for different columns
  curDir <- getwd()
  split_path <- function(x) if (dirname(x)==x) x else c(basename(x),split_path(dirname(x)))
  setwd(file.path(paste(split_path(fileName)[seq(length(split_path(fileName)),2,-1)], collapse = "/")))
  fileName <- split_path(fileName)[1]
  if(length(colsToCount) == 0)
  {
    colsToCount <- seq(1,as.numeric(system(command = paste0("head -1 ",fileName," | awk -F '",fieldSeperator,"' '{print NF}'"), intern = T)),1)
  }
  return(foreach(colNum = colsToCount,.combine = c,.inorder = TRUE) %dopar% system(command = paste0("awk -F '",fieldSeperator,"' '{ total += $", colNum," } END { print total }' ",fileName), intern = T))
}


########################################################################################################################################################

#' Checks if character and numeric values are in same column
#' 
#' This function checks if any of the columns in a data frame has both number and character values. There is potential to capture parsing issue this way, but could also raise lot of false flags.
#' 
#' @param df	Data frame that needs to be checked
#' @return list of names of columns that has both character and numeric values in it.  
#' @export 
#' 

char_numeric_in_same_col <- function(df){
  options(warn=-1)
  # Takes a data frame and returns names of columns that has both character and numeric values in the same column
  charCols <- names(df)[sapply(df, class) == "character"]
  return(charCols[sapply(df[, charCols], function(x) sum(!is.na(as.numeric(x))) > 0)])
  options(warn=0)
}



########################################################################################################################################################

#' Data summary of data frame
#' 
#' This function takes data frame and returns multiple summary statistics. If vintage parameter is provided it also returns p value of anova test done with vintage variable as target variable. This value is very helpful in deducting month on month stability of the variable. A value of 0 indicates high fluctuation. 
#' 
#' @param dat	Data frame for which summary statistics need to be calculated
#' @param vintage	Optional parameter to provide name of the variable in dat that indicates vintage. If this is provided then anova test is also done
#' @return Returns a dataframe with one row for each column containing following summary statistics. Some of these values might not be present depending on the data type of the variable under consideration.            \cr
#'          1. col_name - Name of the column \cr    
#'          2. data_type - Data type deducted by R \cr   
#'          3. Percent_missing - Percentage of rows that are missing  \cr  
#'          4. Unique_values - Number of unique values \cr    
#'          5. Percent_non0_nonNA - Percentage of values that are non null or 0  \cr   
#'          6. Mean - Mean of the column   \cr 
#'          7. SD - Standard deviation \cr    
#'          8. Min - Minumum value   \cr  
#'          9. Max - Maximum value \cr    
#'          10. Anova_p - P value of the anova test done with vintage as target (if vintage parameter is set) \cr
#' @export         
#'          

getSummary <- function(dat, vintage = NULL)
{
  summ <- as.data.frame(matrix(rep(0,10), nrow = 1))
  names(summ) <- c('col_name','data_type','Percent_missing','Unique_values','Percent_non0_nonNA', 'Mean','SD','Min','Max','Anova_p')
  # Finding date columns
  date_fields <- names(grep("POSIX",sapply(dat,class), value = T))
  date_fields <- c(date_fields,names(grep("Date",sapply(dat,class), value = T)) )
  if(length(date_fields) > 0){
    for (i in 1:length(date_fields))
    {
      x <- dat[,date_fields[i]]
      summ[i,1] <- date_fields[i]
      summ[i,2] <- "Date"
      summ[i,3] <- sum(is.na(x))/length(x)
      summ[i,4] <- length(unique(x)) - ifelse(summ[i,3] > 0 ,1,0)
      summ[i,5] <- (sum(!is.na(x)) - sum(x == 0, na.rm = T))/nrow(dat)
      summ[i,6] <- NA
      summ[i,7] <- sd(x, na.rm = T)
      summ[i,8] <- min(x, na.rm = T)
      summ[i,9] <- max(x, na.rm = T)
      summ[i,10] <- NA
    }
    
    dat[,date_fields] <- NULL
  }
  
  logical_fields <- names(grep("logical",sapply(dat,class), value = T))
  allZero <- c()
  if(length(logical_fields) >  0){
    allZero <- logical_fields[sapply(data.frame(dat[,logical_fields]), function(x) sum(!is.na(x)) == 0)]
    logical_fields <- setdiff(logical_fields, allZero)
    if(length(logical_fields) > 0){
      for (col in logical_fields){
        dat[,col] <- dat[,col]*1
      }
    }
  }
  factor_fields <- names(grep("factor",sapply(dat,class), value = T))
  character_fields <- names(grep("character",sapply(dat,class), value = T))
  numeric_fields <- setdiff(names(dat), c(allZero,character_fields, factor_fields))
  
  offset <- ifelse(nrow(summ) == 1,0,nrow(summ))
  if(length(numeric_fields) > 0){
  for (i in 1:length(numeric_fields))
  {
    x <- dat[,numeric_fields[i]]
    summ[i + offset,1] <- numeric_fields[i]
    summ[i + offset,2] <- "numeric"
    summ[i + offset,3] <- sum(is.na(x))/length(x)
    summ[i + offset,4] <- length(unique(x)) - ifelse(summ[i + offset,3] > 0 ,1,0)
    summ[i + offset,5] <- (sum(!is.na(x)) - sum(x == 0, na.rm = T))/nrow(dat)
    summ[i + offset,6] <- mean(x, na.rm = T)
    summ[i + offset,7] <- sd(x, na.rm = T)
    summ[i + offset,8] <- min(x, na.rm = T)
    summ[i + offset,9] <- max(x, na.rm = T)
    if(length(vintage) > 0)
    {
      summ[i + offset,10] <- ifelse(summ[i + offset,5] > 0.05,anova(lm(dat[!is.na(dat[,numeric_fields[i]]),numeric_fields[i]] ~ (dat[!is.na(dat[,numeric_fields[i]]),vintage])))$`Pr(>F)`[1],NA)
    }
  }
  }
  
  # Renaming the logical fields 
  if(length(logical_fields) > 0){
    summ[summ$col_name %in% logical_fields,2] <- "logical"
  }
  
  if(length(character_fields) > 0){
    offset <- nrow(summ)
    for (i in 1:length(character_fields))
    {
      x <- dat[,character_fields[i]]
      summ[i + offset,1] <- character_fields[i]
      summ[i + offset,2] <- "character"
      summ[i + offset,3] <- sum(is.na(x))/length(x)
      summ[i + offset,4] <- length(unique(x)) - ifelse(summ[i + offset,3] > 0 ,1,0)
      summ[i + offset,5] <- NA
      summ[i + offset,6] <- NA
      summ[i + offset,7] <- NA
      summ[i + offset,8] <- NA
      summ[i + offset,9] <- NA
      summ[i + offset,10] <- NA
    }
  }
  
  if(length(factor_fields) > 0){
    offset <- nrow(summ)
    for (i in 1:length(factor_fields))
    {
      x <- dat[,factor_fields[i]]
      summ[i + offset,1] <- factor_fields[i]
      summ[i + offset,2] <- "factor"
      summ[i + offset,3] <- sum(is.na(x))/length(x)
      summ[i + offset,4] <- length(unique(x)) - ifelse(summ[i + offset,3] > 0 ,1,0)
      summ[i + offset,5] <- NA
      summ[i + offset,6] <- NA
      summ[i + offset,7] <- NA
      summ[i + offset,8] <- NA
      summ[i + offset,9] <- NA
      summ[i + offset,10] <- NA
    }
  }
  
  if(length(allZero) > 0){
    offset <- nrow(summ)
    for (i in 1:length(allZero))
    {
      n <- nrow(dat)
      x <- dat[,allZero[i]]
      summ[i + offset,1] <- allZero[i]
      summ[i + offset,2] <- "AllMissing"
      summ[i + offset,3] <- 1
      summ[i + offset,4] <- 0
      summ[i + offset,5] <- NA
      summ[i + offset,6] <- NA
      summ[i + offset,7] <- NA
      summ[i + offset,8] <- NA
      summ[i + offset,9] <- NA
      summ[i + offset,10] <- NA
    }
  }
  if(length(vintage) == 0){ summ$Anova_p <- NULL}
  return(summ)
}



########################################################################################################################################################