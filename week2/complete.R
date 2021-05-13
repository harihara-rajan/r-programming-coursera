complete <- function(directory, id){
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## it should be noted that when data for sulfate is present, 
  ## then we will have data for nitrate as well and therefore 
  ## we can take any one value between nitrate and sulfate.
  
  count <- 1
  nobs <- numeric(length(id))
  for (monitor in id){
    path <- paste(getwd(),"/", directory,"/",sprintf("%03d",monitor) ,".csv",sep = "")
    dataf <- read.csv(path)
    data_nitrate <- dataf[,"nitrate"]
    data_sulfate <- dataf["sulfate"]
    good <- complete.cases(data_nitrate,data_sulfate)
    nobs[count] <- length(data_nitrate[good])
    count <- count + 1
  }
  c <- data.frame(id,nobs)
  c
}