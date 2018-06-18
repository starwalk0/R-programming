corr <- function(directory, treshold = 0) {
        temp <- list.files(directory, pattern = ".csv", full.names = TRUE)
        
        myfiles <- lapply(temp, read.csv)
        
        mydata <- do.call(rbind, myfiles)
        
        newdata <- mydata[which(!(is.na(mydata$sulfate) | is.na(mydata$nitrate | is.na(mydata$Date)))),]
        
        newdata3 <- vector(mode = "numeric")
        
        for (i in 1:323) {
                newdata2 <- newdata[which(newdata$ID == i),]
                numrows <- nrow(newdata2)

                if (numrows >= treshold){
                       ## vec1 <- newdata2$sulfate
                ##        vec2 <- newdata2$nitrate
                        res <- cor(newdata2$sulfate, newdata2$nitrate)

                        newdata3 <- c(newdata3, res)
                }

        }
        newdata3
}