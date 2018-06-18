above10 <- function(x){
        use <- x > 10
        x[use]
}

above <- function(x, n = 10){
        use <- x > n
        x[use]
}

columnmean <- function(y, removeNA = TRUE){
        nc <- ncol(y)
        means <- numeric(nc)
        for(i in 1 : nc){
                means[i] <- mean(y[,i], na.rm = TRUE)
        }
        means
        
}

f <- function(a, b=1, c=2, d=NULL){
        
}

myplot <- function(x,y,type = 1, ...){
        plot(x,y,type = type, ...)
}

make.power <- function(n){
        pow <- function(x){
                x^n
        }
        pow
}

make.NegLogLik <- function(data, fixed=c(FALSE, FALSE)){
        params <- fixed
        function(p){
                params[!fixed] <- p
                mu <- params[1]
                sigma <- params[2]
                a <- -0.5*length(data)*log(2*pi*sigma^2)
                b <- -0.5*sum((data-mu)^2) / (sigma^2)
                -(a+b)
        }
}