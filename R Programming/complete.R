complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  nobs <- integer()
  for (i in id) {
    
    filename <- ""
    directory <- paste(directory,"/",sep="")
    
    if(i/100 >= 1 ) {
      filename <- paste(directory,toString(i),".csv",sep="")
      
    }else if(i/10 >= 1){
      filename <- paste(directory,"0",toString(i),".csv",sep="")
    }else {
      filename <- paste(directory,"00",toString(i),".csv",sep="")
    }
    file_content <- read.csv(filename)
    file_complete_case = file_content[complete.cases(file_content),]
  
    nobs <- c(nobs,nrow(file_complete_case))
    
  }
  data.frame(id,nobs)
  
  
}