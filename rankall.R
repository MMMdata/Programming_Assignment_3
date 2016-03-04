rankall <- function(outcome, num = "best") {
## Read outcome data, separate state column.
  medicare_outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  state <- medicare_outcomes$State
  state <-sort(unique(state))
  hospital <- rep("", length(state))

  ## Check that the outcome is valid for the state in question.
  for (i in 1:length(state)) {
    medicare_outcomes_states<- medicare_outcomes[medicare_outcomes$State==state[i],]
    if (outcome == 'heart attack') {
      death <- as.numeric(medicare_outcomes_states[,11])
    } else if (outcome == 'heart failure') {
      death <- as.numeric(medicare_outcomes_states[,17])
    } else if (outcome == 'pneumonia') {
      death <- as.numeric(medicare_outcomes_states[,23])
    } else {
    stop("invalid outcome")
    }
    
    ## Determine best, worst, or if argument is invalid or no resuts: NA
    death_rank <- rank(death, na.last=NA)

    if (num=="best") {
      best_worst <- 1
    } else if (num =="worst") {
      best_worst <- length(death_rank)
    } else if (num <= length(death_rank) ) {
      best_worst <- num
    } else {
      best_worst <- (NA)
    }
    
  ## For each state, find the hospital of the given rank
    if (is.na(best_worst)) {
        hospital[i] <- NA
      } else {
        hospital[i] <- medicare_outcomes_states$Hospital.Name[order(death, medicare_outcomes_states$Hospital.Name)[best_worst]]
      }
    }

  ## Return a data frame with the hospital names and the abbreviated state name
  return(data.frame(hospital=hospital, state=state))
}
