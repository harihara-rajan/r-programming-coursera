best <- function(state, outcome)
{
  dataf <- read.csv("/home/hari-cms/r-programming-coursera/week4/outcome-of-care-measures.csv",colClasses = "character")
  if (outcome=="heart attack")
  {
    mortality_rate <- dataf[,11]
  }
  else if(outcome == "heart failure")
  {
    mortality_rate <- dataf[,17]
  }
  else if (outcome == "pneumonia")
  {
    mortality_rate <- dataf[,23]
  }
  state_value <- dataf[,7]==state
  mortality_rate_with_na <- mortality_rate[state_value]
  required_value <- vector()
  count <- 1
  for(i in mortality_rate_with_na)
  {
    if (i != "Not Available")
    {
      required_value[count] <- i 
      count <- count + 1
    }
  }
  rv <- min(as.numeric(required_value))
  print(rv)
  complete_hospital_name <- dataf[,2]
  required_hospital_in_city <- complete_hospital_name[state_value]
  ###### 18/05/2021
  # print(mortality_rate_with_na)
  
  ###### 18/05/2021
  # print(rv)
  # print(required_value)
  sub <- which(mortality_rate_with_na==rv)
  best_hospital <- required_hospital_in_city[sub]
  best_hospital
}
