pollutantmean <- function(directory, pollutant, id = 1:332){

        temp <- list.files(directory, pattern = ".csv", full.names = TRUE)

        myfiles <- lapply(temp, read.csv)
        
        mydata <- do.call(rbind, myfiles)
        
        newdata <- mydata[which(mydata$ID %in% id), pollutant]
        
        mean(newdata, na.rm = TRUE)
}
