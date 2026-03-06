# --------------------------------------------------
# Homework 6 - Task 3
# Ranking Hospitals by Outcome in a State
# --------------------------------------------------

rankhospital <- function(state, outcome, num = "best") {
  
  # Reading dataset
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  # Validating outcomes
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  # Checking state
  if (!(state %in% data$State)) {
    stop("invalid state")
  }
  
  # Checking outcome
  if (!(outcome %in% valid_outcomes)) {
    stop("invalid outcome")
  }
  
  # Selecting correct column
  if (outcome == "heart attack") {
    column <- 11
  } else if (outcome == "heart failure") {
    column <- 17
  } else if (outcome == "pneumonia") {
    column <- 23
  }
  
  # Converting outcome column to numeric
  data[, column] <- as.numeric(data[, column])
  
  # Filtering by state
  state_data <- data[data$State == state, ]
  
  # Removing NA values
  state_data <- state_data[!is.na(state_data[, column]), ]
  
  # Ordering by mortality rate then hospital name
  state_data <- state_data[order(state_data[, column], state_data$Hospital.Name), ]
  
  # Determineing ranking
  if (num == "best") {
    return(state_data$Hospital.Name[1])
  }
  
  if (num == "worst") {
    return(state_data$Hospital.Name[nrow(state_data)])
  }
  
  # If numeric rank exceeds number of hospitals
  if (num > nrow(state_data)) {
    return(NA)
  }
  
  # Returning hospital of given rank
  state_data$Hospital.Name[num]
}

# --------------------------------------------------
# Validation Exercise
# --------------------------------------------------

# Valid input : Rank 4th hospital in Texas for heart failure
rankhospital("TX", "heart failure", 4)

# Best hospital : Check " best " ranking
rankhospital("TX", "heart failure", "best")

# Worst hospital : Check " worst " ranking
rankhospital("TX", "heart failure", "worst")

# Exceeding number of hospitals
rankhospital("TX", "heart failure", 10000)
