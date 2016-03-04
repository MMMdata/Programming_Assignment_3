rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data. Set class of columns to character
	outcomes_medicare <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	
  ## Order medicare_outcomes alphabetically ascending, just in case there's a tie for best hospital.
  medicare_outcomes <- medicare_outcomes[order(medicare_outcomes$Hospital.Name),]

  ## Check that state is valid
  if (!state %in% medicare_outcomes$State) {
    stop("Invalid state.")
  }
  
  ## Check that the outcome is valid
  if (outcome == 'heart attack') {
    death <- as.numeric(medicare_outcomes[,11])
  } else if (outcome == 'heart failure') {
    death <- as.numeric(medicare_outcomes[,17])
  } else if (outcome == 'pneumonia') {
    death <- as.numeric(medicare_outcomes[,23])
  } else {
    stop("Invalid outcome.")
  }

  ## Return hospital name in that state with the given rank
  medicare_outcomes <- medicare_outcomes[medicare_outcomes$State==state,]
	
  ## 30-day death rate - determine best, worse, or if argument is invalid or no resuts: NA
	death_rank <- rank(death, na.last=NA)
  
  if (num=="best") {
    best_worst <- 1
  } else if (num =="worst") {
    best_worst <- length(death_rank)
  } else if (num <= length(death_rank) ) {
    best_worst <- num
  } else {
    return(NA)
  }

  return(medicare_outcomes$Hospital.Name[order(death, medicare_outcomes$Hospital.Name)[r]])
}