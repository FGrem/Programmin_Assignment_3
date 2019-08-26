best <- function(state, disease) {
###..Treatment of entry variables
#
#
#..create a vector with diseases
diseases <- c("heart attack", "heart failure", "pneumonia")
#
###..Load outcome csv file
#
outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
#
#..Check that state is valid
#
#if ((is.null(state)) | (missing(state)) | !(is.element(state,outcomes$State))){
if (!(is.element(state,outcomes$State))){
return(message(paste('Error in best("',state , '", "' , disease , '") : invalid state')))}
#
#..Check that disease is valid
#
if ((is.null(disease)) | (missing(disease)) | !(is.element(sapply(disease,tolower),diseases))){
return(message(paste('Error in best("',state , '", "' , disease , '") : invalid outcome')))}
##
#..change variable disease to lower case
disease <- sapply(disease,tolower)
#..Cutting data.frame (only interesting columns)
#     [2] "Hospital.Name"                                              
#     [7] "State"                                                     
#     [11] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"  
#     [17] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
#     [23] "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"    
#
#.. filtering data.frame by State
outcomes <- outcomes[outcomes[,7] == state,]
#
#.. spliting outcomes in files per disease
file_HA <- outcomes[,c(2,11)]
file_HF <- outcomes[,c(2,17)]
file_PN <- outcomes[,c(2,23)]
#..Hospitals that do not have data on a particular outcome should be excluded
#
file_HA <- file_HA[file_HA[,2] != "Not Available",]
file_HF <- file_HF[file_HF[,2] != "Not Available",]
file_PN <- file_PN[file_PN[,2] != "Not Available",]
#
# convert to numeric
file_HA[,2] <- as.numeric(file_HA[,2])
file_HF[,2] <- as.numeric(file_HF[,2])
file_PN[,2] <- as.numeric(file_PN[,2])
#
#..Return hospital name in that state with lowest 30-day death
#..rate
## The best Hospital
if (disease == "heart attack") {
result <- file_HA$Hospital.Name [which.min(file_HA$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)]
message(result)
}
#
if (disease == "heart failure") {
result <- file_HF$Hospital.Name [which.min(file_HF$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)]
message(result)
}
#
if (disease == "pneumonia") { 
result <- file_PN$Hospital.Name [which.min(file_PN$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)]
message(result)
}
#
}