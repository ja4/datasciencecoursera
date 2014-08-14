pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  data <- NULL
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
    if(is.null(data))
      data <- na.omit(file_content[[pollutant]])
    else
      data <- c(data,na.omit(file_content[[pollutant]]))
  }
 
  mean(data)
}