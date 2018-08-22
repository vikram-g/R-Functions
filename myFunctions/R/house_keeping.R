
#' List files and utilization
#' 
#' This function returns a dataframe containing list of all the files and directories found under the given directory (recursive), with file size and last access time.
#' 
#' @param dir	Directory under which to look. The function looks at all the sub directories and files recursively. Defaults to home directory.
#' @return Returns a dataframe with following columns  \cr 
#' 1. fileName - Name of the file/directory; \cr           
#' 2. directory - The file path of the above file;\cr          
#' 3. isDirectory - Logical indicator of whether the "fileName" is a directory. TRUE for directory and FALSE if it is a file;          \cr
#' 4. access_date - The date it was last accessed;  \cr        
#' 5. access_time - The timestamp of last access time;  \cr        
#' 6. fileSize - File size in bytes - which is easier for sorting; \cr          
#' 7. fileSize_formatted - File size in human readable form;  \cr   
#' @export 
#'  

list_files <- function(dir = "~"){
  fileList <- sapply(system(paste0("ls  -Rl --full-time --time=atime ",dir),intern = T), function(x) strsplit(x, "\\s+"))
  fileSumm <- data.frame(fileName = character(), directory = character(), isDirectory = logical(), access_date = character(), access_time = character(),fileSize = integer(), fileSize_formated = character(), stringsAsFactors = F)
  i <- 0
  for (iter in fileList){
    i <- i + 1
    if(length(iter) == 1){curDir <- iter}
    if(length(iter) > 8){fileSumm <- rbind(fileSumm, list(fileName = ifelse(length(iter) == 9, iter[9], paste(iter[9:length(iter)], collapse = " ")), directory = curDir, isDirectory = substr(iter[1],1,1) == "d",
                                                          access_date = iter[6], access_time = iter[7],fileSize = as.numeric(iter[5]), fileSize_formated = utils:::format.object_size(as.numeric(iter[5]), "auto")), stringsAsFactors = F)}
  }
  return(fileSumm)
}

#' Search inside R codes  
#' 
#' This function searches in all the R files under the provided directory and returns list of files that has the search term required. 
#' This is particularly usefull when looking for functions used in past but not sure where the file is or to see if a particular function has been used by anyone else and present under esas folder. 
#' This can search any .R* files.
#' 
#' @param dir Directory under which to look. The function looks at all the sub directories and files recursively. Defaults to home directory.
#' @param search_phrase Search phrase that needs to looked in R files. Regex can also be used. 
#' @param function_declaration Logical value. If set to T then search_phrase is assumed to be function name and the file where it is declared is returned
#' @export 
#' @return A list of all the files which has the given search term
#' 
#' 
#'
search_R_code <- function(dir = "~",search_phrase, function_declaration = F){
  if(function_declaration){search_phrase <- paste0(search_phrase, " ?<- ?function")}
  # Getting all R files
  Rfiles <- list.files(path = dir,pattern = "*\\.R$",recursive = T)
  Rfiles <- paste(dir,"/",Rfiles, sep = "")
  
  # Finding the files with matching search phrase
  match_file <- list()
  for (fn in Rfiles) {
    if (length(grep(search_phrase, readLines(fn)) > 0)) { match_file <- c(match_file,fn)}
  }
  if(length(match_file) == 0){message("No files with the phrase found")}
  return(unlist(match_file))
}