## Put comments here that give an overall description of what your
## functions do

## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse of the matrix
## 4.get the value of the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
          x <<- y
          m <<- NULL
        }
        get <- function() x
        setSolve <- function(solve) m <<- solve
        getSolve <- function() m
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
  
}


## The following function calculates the inverse of the special "matrix" created 
## with the above function. However, it first checks to see if the inverse has already
## been calculated. If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the mean of the data and sets the value of the mean in the cache 
## via the setmean function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getSolve()
        if(!is.null(m)) {
          message("getting cached data")
          return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setSolve(m)
        m
  
}

