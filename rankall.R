rankall <- function(outcome, num = "best") {
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check that outcome is valid
        outcome <- toupper(outcome)
        
        if  (!outcome == "HEART ATTACK" & !outcome == "HEART FAILURE" & !outcome == "PNEUMONIA")
                stop("invalid outcome")    
        
        ## For each state, find the hospital of the given rank

        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        states <- unique(data$State)
        states <- states[order(states)]
        output <- data.frame(Hospital = character(),
                             State = character())
        
        for (st in states){
                
                result <- subset(data, data$State == st, select = c("Hospital.Name", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"))
                names(result) <- c("Hospital", "Heart_Attack", "Pneumonia", "Heart_Failure")
                
                maxn <- nrow(result)
                
                if (num == "best")
                        x <- 1
                else if (num == "worst")
                        x <- maxn
                else 
                        x <- num
                
                if (outcome == "HEART ATTACK") {
                        result$Heart_Attack <- suppressWarnings(as.numeric(result$Heart_Attack))
                        result2 <- subset(result, !is.na(result$Heart_Attack), select = c("Hospital", "Heart_Attack"))
                        
                        sorted <- result2[order(result2["Heart_Attack"], result2["Hospital"]),]
                        if (num == "worst")
                                x <- nrow(result2)
                        
                        res <- data.frame(sorted[x,1], st)
                        output <- rbind(output, res)
                        
                }
                else if (outcome == "HEART FAILURE") {
                        result$Heart_Failure <- suppressWarnings(as.numeric(result$Heart_Failure))
                        result2 <- subset(result, !is.na(result$Heart_Failure), select = c("Hospital", "Heart_Failure"))
                        sorted <- result2[order(result2["Heart_Failure"], result2["Hospital"]),]
                        if (num == "worst")
                                x <- nrow(result2)
                        res <- data.frame(sorted[x,1], st)
                        output <- rbind(output, res)
                }
                else if (outcome == "PNEUMONIA") {
                        result$Pneumonia <- suppressWarnings(as.numeric(result$Pneumonia))
                        
                        result2 <- subset(result, !is.na(result$Pneumonia), select = c("Hospital", "Pneumonia"))
                        
                        sorted <- result2[order(result2["Pneumonia"], result2["Hospital"]),]
                        if (num == "worst")
                                x <- nrow(result2)
                        res <- data.frame(sorted[x,1], st)
                        output <- rbind(output, res)
                }        
        }
        names(output) <- c("hospital", "state")
        output
}