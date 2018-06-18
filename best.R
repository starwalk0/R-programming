best <- function(state, outcome) {

        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check that state and outcome are valid
        states <- data$State
        
        if  (!state %in% states) 
            stop("invalid state")    
        
        outcome <- toupper(outcome)
        
        if  (!outcome == "HEART ATTACK" & !outcome == "HEART FAILURE" & !outcome == "PNEUMONIA")
                stop("invalid outcome")    
        
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        
        if (outcome == "HEART ATTACK") {
                data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- suppressWarnings(as.numeric(data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
                result <- subset(data, data$State == state & !is.na(data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack), select = c("Hospital.Name", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"))
                names(result) <- c("Hospital", "Heart_Attack")
                sorted <- result[order(result["Heart_Attack"], result["Hospital"]),]
                sorted[1,1]
                
        }
        else if (outcome == "HEART FAILURE") {
                data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- suppressWarnings(as.numeric(data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
                result <- subset(data, data$State == state & !is.na(data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure), select = c("Hospital.Name", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"))
                names(result) <- c("Hospital", "Heart_Failure")
                sorted <- result[order(result["Heart_Failure"], result["Hospital"]),]
                sorted[1,1]
        }
        else if (outcome == "PNEUMONIA") {
                data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- suppressWarnings(as.numeric(data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
        
                result <- subset(data, data$State == state & !is.na(data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia), select = c("Hospital.Name", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"))

                names(result) <- c("Hospital", "Pneumonia")
                sorted <- result[order(result["Pneumonia"], result["Hospital"]),]
                sorted[1,1]
                
        }
}