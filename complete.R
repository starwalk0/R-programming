complete <- function(directory, id = 1:332){
        temp <- list.files(directory, pattern = ".csv", full.names = TRUE)
        
        myfiles <- lapply(temp, read.csv)
        
        mydata <- do.call(rbind, myfiles)
        
        newdata <- mydata[which(mydata$ID %in% id & !(is.na(mydata$sulfate) | is.na(mydata$nitrate | is.na(mydata$Date)))),]
        
        vect <- vector("numeric")
        
        for (i in id){
                ##print(paste(i, nrow(newdata[which(newdata$ID == i),])))
                ##nobs(newdata[which(newdata$ID == i),])
                vect <- c(vect, nobs = nrow(newdata[which(newdata$ID == i),]))
        }
        vect
}
