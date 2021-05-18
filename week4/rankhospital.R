rankhospital <- function (state, outcome, num ="best")
{
    data <- read.csv("/home/hari-cms/r-programming-coursera/week4/outcome-of-care-measures.csv",
                     colClasses = "character",)
    if (outcome=="heart attack")
    {
      mortality_rate <- data[,11] # mr data for heartattack
    }
    else if(outcome == "heart failure")
    {
      mortality_rate <- data[,17] # mr data for hear failure
    }
    else if (outcome == "pneumonia")
    {
      mortality_rate <- data[,23] # mr data for pneumonia
    }
    state_value <- data[,7]==state # T/F in state_value. T --> state gn as arg
    tr <- which(state_value==TRUE) # retrieves the pos. of T in state_value
    b <- as.numeric(mortality_rate)# changing from character to numeric
    result <- numeric() # empty vector
    count <- 1
    for (i in tr)
    {
      result[count] <- b[i]
      count <- count + 1
    }
    m_r <- result
    hospital <- data[,2] # takes all the hospital names from the data set 
    required_hospital <- hospital[state_value] # subsetting hopital of state we need 
    d.f <- data.frame(hos = required_hospital, m_r = m_r) # creating D.F
    final_d.f<- na.omit(d.f) # omit all rows that has NA from the data set
    final <- final_d.f[order(final_d.f[,2]),] # https://www.youtube.com/watch?v=TJTkU2yleeU&t=323s
    # print(final)
    if (num=="best")
    {
      best_hosp <- best(state,
                        outcome) #calling best function and getting best hospit.
    }
    else if (num == "worst")
    {
      # print("worst")
      bt <- which(final_d.f[,2]==max(final_d.f[,2]))
      best_hosp <- required_hospital[bt]
    }
    else
    {
      # if there exist a tie
      l <- as.numeric(length(final[,2]))
      # print (num)
      # print(l)
      if(num >= l)
      {
        if (num>l)
        {
          # print("inside if condition in else statement ")
          best_hosp <- NA
        }
        else
        {
          best_hosp <- required_hospital[num]
        }

      }
      else if (final[,2][num] == final[,2][num+1])
      {
        # print("inside else if statement")
        tie_1 <- as.character(final[,1][num])
        tie_2 <- as.character(final[,1][num+1])
        sorted_data <- sort(c(tie_1,tie_2))
        prefered_hos_between_ties <- sorted_data[1]
        best_hosp <- prefered_hos_between_ties
      }
      else if (final[,2][num] == final[,2][num-1])
      {
        # print("inside elseif")
        tie_1 <- as.character(final[,1][num])
        tie_2 <- as.character(final[,1][num-1])
        sorted_data <- sort(c(tie_1,tie_2))
        prefered_hos_between_ties <- sorted_data[2]
        best_hosp <- prefered_hos_between_ties
      }
      # if no tie
      else
      {
        # print("no tie")
        best_hosp <- final[,1][num]
      }
    }
    as.character(best_hosp)
}
