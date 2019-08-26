rankall <- function(outcome, num = "best") {
## For each state, find the hospital of the given rank
## Return a data frame with the hospital names and the
## (abbreviated) state name
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
#..Check that num is valid
#
if ((is.null(num)) | (!(is.numeric(num)) & (sapply(num,tolower) != "best") & (sapply(num,tolower) != "worst"))) {
return(message(paste('Error in
rankall("',outcome , '", "' , num , '") : invalid outcome')))}
if (!(is.numeric(num))) {num <- sapply(num,tolower)}
#
#..Check that disease is valid
#
if ((is.null(outcome)) | (missing(outcome)) | !(is.element(sapply(outcome,tolower),diseases))){
return(message(paste('Error in rankall("',outcome , '", "' , num , '") : invalid outcome')))}
##
#..change variable disease to lower case
outcome <- sapply(outcome,tolower)
#
#..Cutting data.frame (only interesting columns)
#     [2] "Hospital.Name"                                              
#     [7] "State"                                                     
#     [11] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"  
#     [17] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
#     [23] "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"    
#
#.. spliting outcomes in files per disease
file_HA <- outcomes[,c(7,2,11)]
file_HF <- outcomes[,c(7,2,17)]
file_PN <- outcomes[,c(7,2,23)]

#..Hospitals that do not have data on a particular outcome should be excluded
#
file_HA <- file_HA[file_HA[,3] != "Not Available",]
file_HF <- file_HF[file_HF[,3] != "Not Available",]
file_PN <- file_PN[file_PN[,3] != "Not Available",]
#
# convert to numeric
file_HA[,3] <- as.numeric(file_HA[,3])
file_HF[,3] <- as.numeric(file_HF[,3])
file_PN[,3] <- as.numeric(file_PN[,3])
#
if(exists("file_Result_Final")) 
rm("file_Result_Final")
#
#
if (outcome == "pneumonia") {
#
#### BEST
   if (num == "best") {
     for (i in 1:length(levels(factor(outcomes$State)))) {
       file_State <- file_PN[file_PN[,1] == levels(factor(outcomes$State))[i],]
       file_Result <- head(file_State[order(file_State[1],file_State[3]),],1) 
       if(exists("file_Result_Final"))
          file_Result_Final <- rbind(file_Result_Final,file_Result[,c(1,2)])
       else
         file_Result_Final <- file_Result[,c(1,2)]   
    } 
	return(file_Result_Final)
   }
#
#### WORST
#   
   else if (num == "worst") {
     for (i in 1:length(levels(factor(outcomes$State)))) {
       file_State <- file_PN[file_PN[,1] == levels(factor(outcomes$State))[i],]
       file_Result <- tail(file_State[order(file_State[1],file_State[3]),],1) 
       if(exists("file_Result_Final"))
          file_Result_Final <- rbind(file_Result_Final,file_Result[,c(1,2)])
       else
         file_Result_Final <- file_Result[,c(1,2)]    
    }
	return(file_Result_Final)	
   }
   #
#### num a NUMBER
# 
   else if (is.numeric(num)) {
     for (i in 1:length(levels(factor(outcomes$State)))) {
       file_State <- file_PN[file_PN[,1] == levels(factor(outcomes$State))[i],]
       file_Result <- head(file_State[order(file_State[1],file_State[3]),],num) 
       if(exists("file_Result_Final"))
          file_Result_Final <- rbind(file_Result_Final,file_Result[,c(1,2)])
       else
         file_Result_Final <- file_Result[,c(1,2)]
    } 
	return(file_Result_Final)	
   }
  else return(message(paste('Error in rankall("',outcome , '", "' , num , '") : invalid num')))}
#
if (outcome == "heart attack") {
#
#### BEST
   if (num == "best") {
     for (i in 1:length(levels(factor(outcomes$State)))) {
       file_State <- file_HA[file_HA[,1] == levels(factor(outcomes$State))[i],]
       file_Result <- head(file_State[order(file_State[1],file_State[3]),],1) 
       if(exists("file_Result_Final"))
          file_Result_Final <- rbind(file_Result_Final,file_Result[,c(1,2)])
       else
         file_Result_Final <- file_Result[,c(1,2)]   
    } 
	return(file_Result_Final)
   }
#
#### WORST
#   
   else if (num == "worst") {
     for (i in 1:length(levels(factor(outcomes$State)))) {
       file_State <- file_HA[file_HA[,1] == levels(factor(outcomes$State))[i],]
       file_Result <- tail(file_State[order(file_State[1],file_State[3]),],1) 
       if(exists("file_Result_Final"))
          file_Result_Final <- rbind(file_Result_Final,file_Result[,c(1,2)])
       else
         file_Result_Final <- file_Result[,c(1,2)]    
    }
	return(file_Result_Final)	
   }
   #
#### num a NUMBER
# 
   else if (is.numeric(num)) {
     for (i in 1:length(levels(factor(outcomes$State)))) {
       file_State <- file_HA[file_HA[,1] == levels(factor(outcomes$State))[i],]
       file_Result <- head(file_State[order(file_State[1],file_State[3]),],num) 
       if(exists("file_Result_Final"))
          file_Result_Final <- rbind(file_Result_Final,file_Result[,c(1,2)])
       else
         file_Result_Final <- file_Result[,c(1,2)]
    } 
	return(file_Result_Final)	
   }
  else return(message(paste('Error in rankall("',outcome , '", "' , num , '") : invalid num')))}
#
if (outcome == "heart failure") {
#
#### BEST
   if (num == "best") {
     for (i in 1:length(levels(factor(outcomes$State)))) {
       file_State <- file_HF[file_HF[,1] == levels(factor(outcomes$State))[i],]
       file_Result <- head(file_State[order(file_State[1],file_State[3]),],1) 
       if(exists("file_Result_Final"))
          file_Result_Final <- rbind(file_Result_Final,file_Result[,c(1,2)])
       else
         file_Result_Final <- file_Result[,c(1,2)]   
    } 
	return(file_Result_Final)	
   }
#
#### WORST
#   
   else if (num == "worst") {
     for (i in 1:length(levels(factor(outcomes$State)))) {
       file_State <- file_HF[file_HF[,1] == levels(factor(outcomes$State))[i],]
       file_Result <- tail(file_State[order(file_State[1],file_State[3]),],1) 
       if(exists("file_Result_Final"))
          file_Result_Final <- rbind(file_Result_Final,file_Result[,c(1,2)])
       else
         file_Result_Final <- file_Result[,c(1,2)]    
    } 
	return(file_Result_Final)	
   }
   #
#### num a NUMBER
# 
   else if (is.numeric(num)) {
     for (i in 1:length(levels(factor(outcomes$State)))) {
       file_State <- file_HF[file_HF[,1] == levels(factor(outcomes$State))[i],]
       file_Result <- head(file_State[order(file_State[1],file_State[3]),],num) 
       if(exists("file_Result_Final"))
          file_Result_Final <- rbind(file_Result_Final,file_Result[,c(1,2)])
       else
         file_Result_Final <- file_Result[,c(1,2)]
    } 
	return(file_Result_Final)
   }
  else return(message(paste('Error in rankall("',outcome , '", "' , num , '") : invalid num')))}
#
}