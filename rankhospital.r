rankhospital <- function(state, disease, num = "best" ) {
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
return(message(paste('Error in rankhospital("',state , '", "' , disease , '", "' , NUM , '") : invalid state')))}
#
#..Check that disease is valid
#
if ((is.null(disease)) | (missing(disease)) | !(is.element(sapply(disease,tolower),diseases))){
return(message(paste('Error in rankhospital("',state , '", "' , disease , '", "' , NUM , '") : invalid outcome')))}
##
#..Check that num is valid
#
if ((class(num) != "numeric")) { 
   num <- sapply(num,tolower)
   if (num != "best" & num != "worst") {
      return(message(paste('Error in rankhospital("',state , '", "' , disease , '", "' , num , '") : invalid value for num')))}} 
##
#..change variable disease to lower case
disease <- sapply(disease,tolower)
#
#.. filtering data.frame by State
outcomes <- outcomes[outcomes[,7] == state,]
#
#..Cutting data.frame (only interesting columns)
#     [2] "Hospital.Name"                                              
#     [7] "State"                                                     
#     [11] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"  
#     [17] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
#     [23] "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"    
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
# convert to numeric second column(rates)
file_HA[,2] <- as.numeric(file_HA[,2])
file_HF[,2] <- as.numeric(file_HF[,2])
file_PN[,2] <- as.numeric(file_PN[,2])
#
#
#..Return hospital name in that state with lowest 30-day death
#..rate
## 
if (disease == "heart attack") {
   if (num == "best") {
     result <- head(file_HA[order(file_HA[2],file_HA[1]),],1)}
   if (num == "worst"){
	 result <-  tail(file_HA[order(file_HA[2],file_HA[1]),],1)}
   if ((class(num) == "numeric") & nrow(file_HA) > num) {
     result <-  head(file_HA[order(file_HA[2],file_HA[1]),],num)}
   else {message("NA")}
return(result)
} 
## 
if (disease == "heart failure") {
   if (num == "best") {
     result <- head(file_HF[order(file_HF[2],file_HF[1]),],1)}
   if (num == "worst"){
	 result <-  tail(file_HF[order(file_HF[2],file_HF[1]),],1)}
   if ((class(num) == "numeric") & nrow(file_HF) > num) {
     result <-  head(file_HF[order(file_HF[2],file_HF[1]),],num)}
   else {message("NA")}
return(result)
} 
#
if (disease == "pneumonia") {
   if (num == "best") {
     result <- head(file_PN[order(file_PN[2],file_PN[1]),],1)}
   if (num == "worst"){
	 result <-  tail(file_PN[order(file_PN[2],file_PN[1]),],1)}
   if ((class(num) == "numeric") & nrow(file_PN) > num) {
     result <-  head(file_PN[order(file_PN[2],file_PN[1]),],num)}
   else {message("NA")}
return(result[,num])
} 
#
}
