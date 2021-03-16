rankall <- function(outcome, num = "best") {
  ## Read outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")[,c(2,7,11,17,23)]
  
  ## Check that state and outcome are valid
  
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
  
 ## removing unnecessary outcomes
  
  outcome_data<-switch(
    which(outcome_var==outcome),
    outcome_data[,c(1,2,3)],
    outcome_data[,c(1,2,4)],
    outcome_data[,c(1,2,5)]
  )
  
  names(outcome_data)[1] = "hospital"
  names(outcome_data)[2] = "state"
  names(outcome_data)[3] = "deaths"
  
  outcome_data$deaths<- suppressWarnings( as.numeric(outcome_data$deaths) )
  
  data<-outcome_data[order(outcome_data$state,outcome_data$deaths),]
  data<-data[!is.na(data$deaths),]
    
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  
  split_data = split(data, data$state)
  ans = lapply(split_data, function(x, num) {
    # Order by deaths and hospitals
    x = x[order(x$deaths, x$hospital),]
    
    # Return
    if(class(num) == "character") {
      if(num == "best") {
        return (x$hospital[1])
      }
      else if(num == "worst") {
        return (x$hospital[nrow(x)])
      }
    }
    else {
      return (x$hospital[num])
    }
  }, num)
  
  return ( data.frame(hospital=unlist(ans), state=names(ans)) )
  
  
}