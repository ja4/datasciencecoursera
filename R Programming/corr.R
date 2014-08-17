corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  corr_values <- numeric()
  files = list.files(directory)
  
  for(file in files)
  {
    directory <- paste(directory,"/",sep="")
    filename <- paste(directory,file,sep="")
    
    file_content <- read.csv(filename)
    file_complete_case = file_content[complete.cases(file_content),]
    if(nrow(file_complete_case) > threshold) {
      corr_values <- c(corr_values,cor(file_complete_case$sulfate,file_complete_case$nitrate))
    }
  }
  corr_values
}