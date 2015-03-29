##Programming assignment 3 - hospital data
setwd("~/R_coursera/ProgrammingAssignment3")

##part1 - plotting 30 day mortality rates for heart attacks
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
ncol(outcome) #number of columns or variables in dataset
nrow(outcome)
names(outcome) #names of columns/variables

outcome[, 11] <- as.numeric(outcome[, 11]) #11th column is heart attack variable
hist(outcome[, 11]) #histogram of the 30 day mortality rates for heart attack


##part2 - finding the best hospital in a state
best <- function(state, outcome) {
  #Read outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  #Check if state and outcome are valid
  state_list <- unique(outcome_data[, 7]) # unique returns a vector that contains all the unique elements in the 7th column of the outcome data
  if(state %in% state_list == FALSE){
    stop("invalid state")
  }
  if(outcome != "heart attack" & outcome != "heart failure" & outcome != "pneumonia"){
    stop("invalid outcome")
  }
  
  #Return hospital name in that state with lowest 30-day mortality rate
  #Find the column number
  col_num <- ifelse(outcome == "heart attack", 11, ifelse(outcome == "heart failure", 17, 23))
  
  #Make a dataset with three columns: Hospital Name, State, and Outcome and also drop all rows that are not for the given state 
  names(outcome_data)[col_num] <- "Outcome"
  #Renames the third column "Outcome"
  
  outcome_subset <- subset(outcome_data, State == state & Outcome != "Not Available", select = c(2, 7, col_num))
  outcome_subset[, 3] <- as.numeric(outcome_subset[, 3])
  # To sort by the outcome, first convert the values in that column to numeric values
  
  outcome.order <- with(outcome_subset, order(outcome_subset$Outcome, outcome_subset$Hospital.Name))
  # Sorts the data frame outcome_subset first by the number in the column "Outcome" and then by "Hospital.Name"
  Outcome_Subset_Sorted <- outcome_subset[outcome.order, ]
  
  best_hospital <- Outcome_Subset_Sorted[1, 1]
  best_hospital #Return the name of the hospital with the best outcome
}

##returns the top 10 hospitals for best outcome
top.10 <- function(state, outcome) {
 
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  state_list <- unique(outcome_data[, 7])
  
  if(state %in% state_list == FALSE){
    stop("invalid state")
  }
  if(outcome != "heart attack" & outcome != "heart failure" & outcome != "pneumonia"){
    stop("invalid outcome")
  }
  
  col_num <- ifelse(outcome == "heart attack", 11, ifelse(outcome == "heart failure", 17, 23))
  names(outcome_data)[col_num] <- "Outcome"  
  outcome_subset <- subset(outcome_data, State == state & Outcome != "Not Available", select = c(2, 7, col_num))
  outcome_subset[, 3] <- as.numeric(outcome_subset[, 3])
  
  outcome.order <- with(outcome_subset, order(outcome_subset$Outcome, outcome_subset$Hospital.Name))
  Outcome_Subset_Sorted <- outcome_subset[outcome.order, ]
  
  top_10 <- Outcome_Subset_Sorted[1:10, 1]
  top_10
}

#test cases
best("WA","heart attack")
best("IL","pneumonia")

top.10("WA","heart attack")
top.10("IL","pneumonia")


##part 3 - ranking hospitals by outocme in a state
#this function would give the hospital given rank number, state, and medical condition
rankhospital <- function(state, outcome, num = "best") {
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  state_list <- unique(outcome_data[, 7])
  
  if(state %in% state_list == FALSE){
    stop("invalid state")
  }
  if(outcome != "heart attack" & outcome != "heart failure" & outcome != "pneumonia"){
    stop("invalid outcome")
  }
  
  col_num <- ifelse(outcome == "heart attack", 11, ifelse(outcome == "heart failure", 17, 23))
  names(outcome_data)[col_num] <- "Outcome"
  
  outcome_subset <- subset(outcome_data, State == state & Outcome != "Not Available", select = c(2, 7, col_num))
  outcome_subset[, 3] <- as.numeric(outcome_subset[, 3])
  outcome.order <- with(outcome_subset, order(outcome_subset$Outcome, outcome_subset$Hospital.Name))
  Outcome_Subset_Sorted <- outcome_subset[outcome.order, ]
  Outcome_Subset_Sorted$rank <- rank(Outcome_Subset_Sorted$Outcome, ties.method = "first")
 
  max <- max(Outcome_Subset_Sorted$rank)
  if(num == "best"){num <- 1}
  if(num == "worst"){num <- max}
  
  desired_hospital <- Outcome_Subset_Sorted[num, 1]
  desired_hospital
}

#test cases
rankhospital("WA","heart attack",7)
rankhospital("WA","heart failure","best")
rankhospital("IL","heart attack",3)
rankhospital("IL","heart failure","worst")
rankhospital("CA","heart attack",9)
rankhospital("CA","pneumonia","best")
rankhospital("CA","heart failure","worst")


##part 4 - ranking hospitals in all states
#returns a 2-column data frame containing the hospital in each state that has the ranking specified
rankall <- function(outcome, num = "best") {
  
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
 
  if(outcome != "heart attack" & outcome != "heart failure" & outcome != "pneumonia"){
    stop("invalid outcome")
  }
  
  col_num <- ifelse(outcome == "heart attack", 11, ifelse(outcome == "heart failure", 17, 23))
  names(outcome_data)[col_num] <- "Outcome"
   
  outcome_subset <- subset(outcome_data, Outcome != "Not Available", select = c(2, 7, col_num))
  outcome_subset[, 3] <- as.numeric(outcome_subset[, 3])
  outcome.order <- with(outcome_subset, order(outcome_subset$State, outcome_subset$Outcome, outcome_subset$Hospital.Name))
  Outcome_Subset_Sorted <- outcome_subset[outcome.order, ]        
  state_outcome_data <- split(Outcome_Subset_Sorted, Outcome_Subset_Sorted$State)
      
  hospitals_by_rank <- sapply(state_outcome_data, function(x){
    x$rank <- rank(x$Outcome, ties.method = "first")
    max <- max(x$rank)
    if(num == "best"){num <- 1}
    if(num == "worst"){num <- max}
    x$Hospital.Name[num]
  })
  
  df <- data.frame(hospital = hospitals_by_rank)
  df$state <- row.names(df)
  
  df
}

##test cases
rankall("heart attack",1)
rankall("heart attack","best")
head(rankall("heart attack",1),10)
