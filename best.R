best <- function(state, outcome){
  ## Read outcome data
  medicare_outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

  ## Order medicare_outcomes alphabetically ascending, just in case there's a tie for best hospital.
  medicare_outcomes <- medicare_outcomes[order(medicare_outcomes$Hospital.Name)]

  ## Check that state and outcome are valid
  if (!state %in% medicare_outcomes$State) {
    stop("invalid state")
  }
  ## Return hospital name in that state with lowest 30-day death rate
  medicare_outcomes <- medicare_outcomes[medicare_outcomes$State==state,]
  if (outcome == 'heart attack') {
    death <- as.numeric(medicare_outcomes[,11])
  } else if (outcome == 'heart failure') {
    death <- as.numeric(medicare_outcomes[,17])
  } else if (outcome == 'pneumonia') {
    death <- as.numeric(medicare_outcomes[,23])
  } else {
    stop("invalid outcome")
  }
  ## Row number(s) of best hospital
  best_hospital <- which(death == min(death, na.rm=T))

  return(medicare_outcomes$Hospital.Name[best_hospital])
}
