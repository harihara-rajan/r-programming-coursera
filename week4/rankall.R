rankall <- function(outcome, num="best")
{
    data <- read.csv("/home/hari-cms/r-programming-coursera/week4/outcome-of-care-measures.csv",
                     colClasses = "character")
    if (outcome == "heart attack")
    {
        mr_with_na <- data[,11]
    }
    else if (outcome == "heart failure")
    {
        mr_with_na <- data[,17]
    }
    else if (outcome == "pneumonia")
    {
        mr_with_na <- data[,23]
    }
    state_list <-as.character(unique(factor(data[,7]))) 
    best_hospital_in_each_state <- vector()
    count <- 1
    for (i in 1:length(state_list))
    {
        # print(state_list[i])
        b <- rankhospital(state_list[i],outcome,num)
        # print(b)
        best_hospital_in_each_state[count] <- b
        count <- count + 1
    }
    #creating Data Frame
    ranklist <- data.frame(Hospital=best_hospital_in_each_state,
                           State=state_list)
    ranklist
}