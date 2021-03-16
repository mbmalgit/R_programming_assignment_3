rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")[,c(2,7,11,17,23)]
  
  ## Check that state and outcome are valid
  if(!(state %in% outcome_data$State)){
    stop ("invalid state")
  }
  ## check outcome
  outcome_var<-c("heart attack","heart failure","pneumonia")
  ##return(outcome %in% outcome_var)
  if(!(outcome %in% outcome_var)){
    stop ("invalid outcome")
  }
  
  if(class(num) == "character"){
    if (!(num %in% c("best","worst"))){
      stop("invalid number")
    }
  }
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  
  outcome_data=outcome_data[outcome_data$State==state,c(1,3,4,5)]
  
  outcome_data<-switch(
    which(outcome_var==outcome),
    outcome_data[,c(1,2)],
    outcome_data[,c(1,3)],
    outcome_data[,c(1,4)]
  )
  names(outcome_data)[2] = "Cases"
  outcome_data[, 2] = suppressWarnings( as.numeric(outcome_data[, 2]))
  outcome_data = outcome_data[!is.na(outcome_data$Cases),]
  if(class(num) == "numeric" && num > nrow(outcome_data)){
    return (NA)
  }
  
  outcome_data = outcome_data[order(outcome_data$Cases, outcome_data$Hospital.Name),]
  # Return
  
  if(class(num) == "character") {
  
      if(num == "best") {
      return (outcome_data$Hospital.Name[1])
        }
      else if(num == "worst") {
      return (outcome_data$Hospital.Name[nrow(outcome_data)])
      }
  }
  
  else {
    return (outcome_data$Hospital.Name[num])
  }
  
}