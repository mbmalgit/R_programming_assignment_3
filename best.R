best <- function(state, outcome) {
  ## Read outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  outcome_data<-outcome_data[order(outcome_data$Hospital.Name),]
  
  outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack<-as.numeric(outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
  outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure<-as.numeric(outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
  outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia<-as.numeric(outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
  
  outcome_has_ha<-subset(outcome_data, (!is.na(outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)))
  outcome_has_hf<-subset(outcome_data, (!is.na(outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)))
  outcome_has_pn<-subset(outcome_data, (!is.na(outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)))
  
  
  ## Check that state and outcome are valid
  ## check state
  if(!(state %in% outcome_data$State)){
    stop ("invalid state")
    }
  
  ## check outcome
  outcome_var<-c("heart attack","heart failure","pneumonia")
  ##return(outcome %in% outcome_var)
  if(!(outcome %in% outcome_var)){
    stop ("invalid outcome")
  }

  ## Return hospital name in that state with lowest 30-day death
  ## rate
  val <- switch( 
    which(outcome_var==outcome), 
    
    outcome_has_ha[outcome_has_ha$State==state & outcome_has_ha$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack == min(outcome_has_ha[outcome_has_ha$State==state,]$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),]$Hospital.Name[1],
    outcome_has_hf[outcome_has_hf$State==state & outcome_has_hf$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure == min(outcome_has_hf[outcome_has_hf$State==state,]$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),]$Hospital.Name[1], 
    outcome_has_pn[outcome_has_pn$State==state & outcome_has_pn$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia == min(outcome_has_pn[outcome_has_pn$State==state,]$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),]$Hospital.Name[1]
    
  ) 
  return(val)
  
  
  
}
