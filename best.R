# --------------------------------------------------
# Homework 6 - Task 2
# Finding the Best Hospital in a State
# --------------------------------------------------

best <- function(state, outcome) {
  
  # Reading the dataset
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  # Validating outcomes
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  # Checking if state is valid
  if (!(state %in% data$State)) {
    stop("invalid state")
  }
  
  # Checking if outcome is valid
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
  
  # Ordering by mortality rate and hospital name
  state_data <- state_data[order(state_data[, column], state_data$Hospital.Name), ]
  
  # Returning best hospital
  state_data$Hospital.Name[1]
}

# --------------------------------------------------
# Validation Exercise
# --------------------------------------------------

# Valid input : Check best hospital in Texas for heart attack
best("TX", "heart attack")

# Invalid state : Check error handling
best("ZZ", "heart attack")

# Invalid outcome : Check error handling
best("TX", "wrong outcome")