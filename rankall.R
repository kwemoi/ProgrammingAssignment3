## 4 Ranking hospitals in all states

# Write a function called rankall that takes two arguments:
# an outcome name (outcome) and a hospital ranking (num). 
# The function reads the outcome-of-care-measures.csv file 
# and returns a 2-column data frame
# containing the hospital in each state that has the ranking specified in num.
# For example the function call
# rankall("heart attack", "best") would return a data frame containing the
# names of the hospitals that are the best in their respective states 
# for 30-day heart attack death rates. The function should return a value
# for every state (some may be NA). The first column in the data frame is 
# named hospital, which contains the hospital name, and the second column is
# named state, which contains the 2-character abbreviation for
# the state name. Hospitals that do not have data on a particular outcome 
# should be excluded from the set of hospitals when deciding the rankings.

# Handling ties. The rankall function should handle ties in 
# the 30-day mortality rates in the same way that the
# rankhospital function handles ties.



rankall <- function(outcome, num = "best") {
    ## Read outcome data
    my_data<-read.csv("outcome-of-care-measures.csv",
                      na.strings = "Not Available", stringsAsFactors = FALSE)
    outcomes<-c("heart attack"=11, "heart failure"=17, "pneumonia"=23)
    ## Check that state and outcome are valid
    # if (!state %in% my_data$State){
    #     stop("invalid state")
    # }
    if (!outcome %in% names(outcomes)){
        stop("invalid outcome")
    }
    ## subset by outcome
    mysubset<- my_data[,c(2,7,outcomes[outcome])]
    names(mysubset)<- c( "hospital","state", outcome)
    complete<- na.omit(mysubset)
    ## order by outcome,then hospital
    ranking<-complete[order(complete[[outcome]], complete$hospital),]
   
     ## For each state, find the hospital of the given rank
     ## Split the subset by state /group by state
    mysplit<- split(ranking, ranking$state)

    ## apply function
    hospitalname<- function(data,num){
        # num argument
        if (num== "best"){
            num<- 1
        } else if (num == "worst"){
            num<- nrow(data)
        } else{
            num
        } 
        return(data[num,1])
    }
    
    apply_data<- sapply(mysplit, hospitalname,num)
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    final_data<- data.frame(row.names = names(apply_data), hospital= apply_data
                            , state= names(apply_data))
}