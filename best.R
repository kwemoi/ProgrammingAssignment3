# 1
## Read the data
## Subset to three columns
## Remove NA Values
## Order by state then outcome then hospital name
## Split by state
## Run lapply

## DATA SOURCE
data_url<-" http://hospitalcompare.hhs.gov"

# READ IN DATA
# as character
hospital_data<- read.csv("hospital-data.csv", colClasses = "character")
object.size(hospital_data)
head(hospital_data)
dim(hospital_data)
str(hospital_data)

outcome_measure<- read.csv("outcome-of-care-measures.csv", colClasses = "character")
str(outcome_measure)
trial<-c(11,17,23)
outcome_measure<-outcome_measure[,c(2,7,11,17,23)]
# a histogram plot of 11th  variable
# coerce it to numeric since we read in data as character
outcome_measure[,11]<- as.numeric(outcome_measure[,11])
hist(outcome_measure[,11])
neumonia<-outcome_measure[,23]
neumonia<-as.numeric(neumonia)
neumonia<-na.omit(neumonia)
summary(neumonia)
head(neumonia)

## 2 Finding the best hospital in a state

# Write a function called best that take two arguments: 
# the 2-character abbreviated name of a state and an outcome name. 
# The function reads the outcome-of-care-measures.csv file and returns 
# a character vector with the name of the hospital that has the 
# best (i.e. lowest) 30-day mortality for the specified outcome in that state.
# The hospital name is the name provided in the Hospital.Name variable. 
# The outcomes can be one of “heart attack”, “heart failure”, or “pneumonia”.
# Hospitals that do not have data on a particular outcome should be excluded 
# from the set of hospitals when deciding the rankings.

# Handling ties. If there is a tie for the best hospital for a given outcome,
# then the hospital names should be sorted in alphabetical order and the first
# hospital in that set should be chosen (i.e. if hospitals “b”, “c”,
#and “f” are tied for best, then hospital “b” should be returned)

best <- function(state, outcome) {
    ## Read outcome data
    my_data<-read.csv("outcome-of-care-measures.csv", 
                      na.strings = "Not Available", stringsAsFactors = FALSE)
    

    outcomes <- c("heart attack"=11, "heart failure"=17, "pneumonia"=23) 
    ## Check that state and outcome are valid
     if(!state %in% my_data$State){
        stop("invalid state")
    }
    if(!outcome %in% names(outcomes)){
        stop("invalid outcome")
    }
   
    ## Create the specific sub-dataset 
    Statedata<-subset(my_data, State == state)
    StateHospitals<-Statedata[,c(2,outcomes[outcome])]
    names(StateHospitals)=c("hospital",outcome)
    
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    mybest<-StateHospitals[order(StateHospitals[[outcome]], 
                                 StateHospitals$hospital),]
    mybest<-na.omit(mybest)
    head(mybest)
   return(mybest$hospital[1])
    return(mybest[1, "hospital"])
}

# 1
best("SC", "heart attack")

# 2
best("NY", "pneumonia")
# 3
best("AK", "pneumonia")
