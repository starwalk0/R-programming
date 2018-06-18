rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check that state and outcome are valid
        states <- data$State
        
        if  (!state %in% states) 
                stop("invalid state")    
        
        outcome <- toupper(outcome)
        
        if  (!outcome == "HEART ATTACK" & !outcome == "HEART FAILURE" & !outcome == "PNEUMONIA")
                stop("invalid outcome")    
        
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        
        result <- subset(data, data$State == state, select = c("Hospital.Name", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"))
        names(result) <- c("Hospital", "Heart_Attack", "Pneumonia", "Heart_Failure")
        
        maxn <- nrow(result)
        
        if (num == "best")
                x <- 1
        else if (num == "worst")
                x <- maxn
        else if (num > maxn)
                return (NA)
        else 
                x <- num
        
        if (outcome == "HEART ATTACK") {
                result$Heart_Attack <- suppressWarnings(as.numeric(result$Heart_Attack))
                result2 <- subset(result, !is.na(result$Heart_Attack), select = c("Hospital", "Heart_Attack"))

                sorted <- result2[order(result2["Heart_Attack"], result2["Hospital"]),]
                if (num == "worst")
                        x <- nrow(result2)
                sorted[x,1]
                
        }
        else if (outcome == "HEART FAILURE") {
                result$Heart_Failure <- suppressWarnings(as.numeric(result$Heart_Failure))
                result2 <- subset(result, !is.na(result$Heart_Failure), select = c("Hospital", "Heart_Failure"))
                sorted <- result2[order(result2["Heart_Failure"], result2["Hospital"]),]
                if (num == "worst")
                        x <- nrow(result2)
                sorted[x,1]
        }
        else if (outcome == "PNEUMONIA") {
                result$Pneumonia <- suppressWarnings(as.numeric(result$Pneumonia))
                
                result2 <- subset(result, !is.na(result$Pneumonia), select = c("Hospital", "Pneumonia"))
                
                sorted <- result2[order(result2["Pneumonia"], result2["Hospital"]),]
                if (num == "worst")
                        x <- nrow(result2)
                sorted[x,1]
        }
}
