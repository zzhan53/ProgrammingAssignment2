## Put comments here that give an overall description of what your
## functions do
## The functions improve the efficiency of inverse caculation for matrix, 
## This is the Assignment2 for R Programing class.

## Write a short comment describing this function
## makeVector creates a special "vector", which is really a list
## containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
## The following function calculates the mean of the special "matrix"
## created with the above function. However, it first checks to see if 
## the inverse has already been calculated. If so, it gets the inverse
## from the cache and skips the computation. Otherwise, it calculates
## the inverse of the data and sets the value of the mean in the cache 
## via the setinv function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)){
                message("getting Cached data") # check if the inverse is in cache
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...) # caculate the inverse
        x$setinv(inv)
        inv
}
