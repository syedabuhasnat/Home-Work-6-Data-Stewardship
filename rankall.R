# --------------------------------------------------
# Homework 6 - Task 4
# Ranking Hospitals Across All States
# --------------------------------------------------

rankall <- function(outcome, num = "best") {
  
  # Reading dataset
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  # Validating outcome 
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
  
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
  data[, column] <- suppressWarnings(as.numeric(data[, column]))
  
  # Filtering out NA values for the specific outcome
  data <- data[!is.na(data[, column]), ]
  
  # Splitting the data frame by state to evaluate each state independently
  split_data <- split(data, data$State)
  
  # Applying a function to each state's data to find the right hospital
  hospitals <- lapply(split_data, function(state_data) {
    
    # Ordering by mortality rate then hospital name to handle ties 
    state_data <- state_data[order(state_data[, column], state_data$Hospital.Name), ]
    
    # Determining ranking index
    if (num == "best") {
      rank_num <- 1
    } else if (num == "worst") {
      rank_num <- nrow(state_data)
    } else {
      rank_num <- as.numeric(num)
    }
    
    # If the requested rank exceeds the number of hospitals, return NA
    if (rank_num > nrow(state_data)) {
      return(NA)
    }
    
    # Returning the hospital name
    return(state_data$Hospital.Name[rank_num])
  })
  
  # Formatting output into a data frame
  result_df <- data.frame(
    hospital = unlist(hospitals), 
    state = names(hospitals), 
    row.names = names(hospitals)
  )
  
  return(result_df)
}

# --------------------------------------------------
# Validation Exercise
# --------------------------------------------------

# Best hospitals for heart attack across all states
head(rankall("heart attack", "best"), 10)

# 20th ranked hospital for heart attack across all states
head(rankall("heart attack", 20), 10)

# Missing data handling
tail(rankall("heart failure", "worst"), 5)