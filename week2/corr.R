corr <- function(directory,threshold=0){
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## threshold is a numeric vector of  length 1 indicating the 
  ##number of completely observed observation (on all 
  ## variables) required to complete the correlation between 
  ## nitrate and sulfate; the default value is 0.
  
  count <- 1
  id = 1:332
  need <- numeric(length(id))
  cr <- vector()
  for (monitor in id)
  {
    path <- paste(getwd(),"/", directory,"/",sprintf("%03d",monitor) ,".csv",sep = "")
    dataf <- read.csv(path)
    data_sulfate <- dataf[,"sulfate"]
    data_nitrate <- dataf[,"nitrate"]
    a <- complete.cases(data_nitrate,data_sulfate)
    good_sulfate <- data_sulfate[a]
    good_nitrate <- data_nitrate[a]
    if (length(good_sulfate)>threshold)
    {
      crel <- cor(good_sulfate, good_nitrate)
      cr[count] <- crel
      count <- count + 1
    }
  }
  final <- cr
  final
}