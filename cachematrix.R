## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y    
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)    
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) { 
                message("getting cached data")
                return(m)
        }

        data <- x$get()  
        m <- solve(data) 
        x$setInverse(m)  
        m                
}

## I used this code to test it:
## x <- matrix(rnorm(9), 3,3)
## m <- makeCacheMatrix(x)
## See the results of m and m$get()
## See the results of cacheSolve(m) and m$getInverse()