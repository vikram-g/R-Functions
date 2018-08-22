#Get a percentage string from a decimal
#' Decimal to percent 
#' 
#' Function takes a decimal value and returns it formatted as percentage.
#' 
#' @param x	integer or list of interger that need to be formated as percentage
#' @param digits	Number of digits after decimal point to retain. Defaults to 2
#' @return The list of decimal values formatted as percentage string. 
#' @export 
#' 

decimal_to_percent <- function(x, digits = 2) {
  paste0(formatC(x*100, format = "f", digits = digits),"%")
}


# Current time stamp - generally to append to files
#' Current time stamp
# 
#' This function when called returns the current time stamp. This can be generally used to append time stamp to file names when writing them multiple times without over writing. 
#' @param format Format of the timestamp to be returned
#' @return timestamp
#' @export 
#'  
timestamp <- function(format = "%y%m%d%H%M%S")
{
  strftime(Sys.time(), format)
}

#' @export 
# Not in function
'%ni%' <- function(x,y)!('%in%'(x,y))


#' Match strings that vary slightly
#' 
#' Often the same bureau variables have different names depending on the data source. This function matches these subtly different names using restricted Damerau-Levenshtein string distnace and returns top 5 matches for each name. \cr
#' Note: Always ensure that generally longer version of string names are in nm1. Match rate turns bad if this condition is not met. 
#' @param nm1	List of names that needs to be matched with nm2. Ensure that nm1 is generally longer in length than nm2.
#' @param nm2	List of names that needs to be matched with nm1
#' @return Data frame with one row for each names in first parameter nm1 and 5 columns with the best 5 matches for each of them.
#' @export 
#' 
match_names <- function(nm1, nm2){
  # Removing any "_" or "." in the names
  nm1_fixed <- unname(sapply(nm1, function(x) gsub("_|\\.","",x)))
  nm2_fixed <- unname(sapply(nm2, function(x) gsub("_|\\.","",x))) 
  
  
  res <- data.frame(var = character(), match1 = character(),match2 = character(), match3 = character(), match4 = character(), match5 = character(),stringsAsFactors = F)
  for (i in 1:length(nm2)){
    distances <- stringdist::stringdist(unname(sapply(nm2_fixed[i], replace_nums_to_char))[1], unname(sapply(nm1_fixed, replace_nums_to_char)), method = "osa")
    res[i,] <- c(nm2[i], nm1[order(distances)[1:5]])
  }
  return(res)
}

replace_nums_to_char <- function(str_to_replace){
  nums_in_str <- unlist(strsplit(unlist(str_to_replace), "[^0-9]+")) # Picking the numbers in the string
  nums_in_str <- nums_in_str[order(nums_in_str,decreasing = T)] # Sorting by number of digits so numbers with more digits are replaced before smaller ones
  nums_in_str <- nums_in_str[nchar(nums_in_str) > 0] # Removing empty string
  for (x in nums_in_str){ str_to_replace <- gsub(x,numbers2words(as.numeric(x)),str_to_replace)}
  return (str_to_replace)
}

# Function to convert numbers to string equivalent from here https://gist.github.com/psychemedia/150cb9901529da58124a
numbers2words <- function(x){
  helper <- function(x){
    
    digits <- rev(strsplit(as.character(x), "")[[1]])
    nDigits <- length(digits)
    if (nDigits == 1) as.vector(ones[digits])
    else if (nDigits == 2)
      if (x <= 19) as.vector(teens[digits[1]])
    else trim(paste(tens[digits[2]],
                    Recall(as.numeric(digits[1]))))
    else if (nDigits == 3) trim(paste(ones[digits[3]], "hundred and", 
                                      Recall(makeNumber(digits[2:1]))))
    else {
      nSuffix <- ((nDigits + 2) %/% 3) - 1
      if (nSuffix > length(suffixes)) stop(paste(x, "is too large!"))
      trim(paste(Recall(makeNumber(digits[
        nDigits:(3*nSuffix + 1)])),
        suffixes[nSuffix],"," ,
        Recall(makeNumber(digits[(3*nSuffix):1]))))
    }
  }
  trim <- function(text){
    #Tidy leading/trailing whitespace, space before comma
    text=gsub("^\ ", "", gsub("\ *$", "", gsub("\ ,",",",text)))
    #Clear any trailing " and"
    text=gsub(" and$","",text)
    #Clear any trailing comma
    gsub("\ *,$","",text)
  }  
  makeNumber <- function(...) as.numeric(paste(..., collapse=""))     
  #Disable scientific notation
  opts <- options(scipen=100) 
  on.exit(options(opts)) 
  ones <- c("", "one", "two", "three", "four", "five", "six", "seven",
            "eight", "nine") 
  names(ones) <- 0:9 
  teens <- c("ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen",
             "sixteen", " seventeen", "eighteen", "nineteen")
  names(teens) <- 0:9 
  tens <- c("twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty",
            "ninety") 
  names(tens) <- 2:9 
  x <- round(x)
  suffixes <- c("thousand", "million", "billion", "trillion")     
  if (length(x) > 1) return(trim(sapply(x, helper)))
  helper(x)
}