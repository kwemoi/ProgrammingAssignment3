## Ranking Hospitals by Outcome in a State

# Write a function called rankhospital that takes three arguments: 
# the 2-character abbreviated name of a state (state), an outcome (outcome), 
# and the ranking of a hospital in that state for that outcome (num).
# The function reads the outcome-of-care-measures.csv file and returns a 
# character vector with the name of the hospital that has the ranking specified 
# by the num argument. For example, the call
# rankhospital("MD", "heart failure", 5)
# would return a character vector containing the name of the hospital with 
# the 5th lowest 30-day death rate for heart failure. The num argument can 
# take values “best”, “worst”, or an integer indicating the ranking
# (smaller numbers are better). If the number given by num is larger than
# the number of hospitals in that state, then the function should return NA.
# Hospitals that do not have data on a particular outcome should
# be excluded from the set of hospitals when deciding the rankings.

# Handling ties. It may occur that multiple hospitals have the same 30-day 
# mortality rate for a given cause of death. In those cases ties should be 
# broken by using the hospital name (alphabetical order). 

rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    my_data<-read.csv("outcome-of-care-measures.csv", 
                      na.strings = "Not Available", stringsAsFactors = FALSE)
  outcomes<-c("heart attack"=11, "heart failure"=17, "pneumonia"=23)
    ## Check that state and outcome are valid
  if (!state %in% my_data$State) {
      stop("invalid state")
  }
  if (!outcome %in% names(outcomes)) {
      stop("invalid outcome")
  }
  
    ## subset 
  by_state<- subset(my_data, State== state)
  statehospitals<- by_state[,c(2,outcomes[outcome])]
  names(statehospitals)<- c("hospital", outcome)
  complete<-na.omit(statehospitals)
  ranking<-complete[order(complete[[outcome]],
                          complete$hospital) ,]
  # na.omit(ranking)
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
  last<-nrow(ranking)
  
  if (num== "best") {
      num<- 1
  } else if (num== "worst"){
          num<- last} else {
              num}
  
  # tail(ranking)
  # return(ranking$hospital[num])
  return(ranking[num, "hospital"])
}

# 4
rankhospital("NC", "heart attack", "worst")
# 5
rankhospital("WA", "heart attack", 7)
# 6
rankhospital("TX", "pneumonia", 10)
# 7
rankhospital("NY", "heart attack", 7)