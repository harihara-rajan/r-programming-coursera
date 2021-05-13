pollutantmean<- function(directory, pollutant, id){
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of  the pollutant for which we will calcultate the
  ## mean; either "sulfate" or "nitrate"
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list  
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result
  count = 1                             # counter like variable
  temp2 <- numeric()                     # empty vector for storing the mean value of each monitor id 
  for (monitor in id){
    path <- paste(getwd(),"/", directory,"/",sprintf("%03d",monitor ),".csv",sep = "")
    data <- read.csv(path)              # data frame
    need <- data[pollutant]            # extracting only data that is of interest 
    bad <- is.na(need)
    useful_data <- need[!bad]
    for (i in useful_data){
      temp2[count] <- i
      count <- count + 1
    }
  }
  mean(temp2)
}