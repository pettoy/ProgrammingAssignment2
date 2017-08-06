
## Matrix inversion is usually a costly computation and there may be 
##    some benefit to caching the inverse of a matrix rather than 
##    compute it repeatedly. The pair of functions written below cache
##    the inverse of a matrix.

## The first function, makeCacheMatrix, creates a special "matrix" object 
##    that can cache its inverse.
## The makeCacheMatrix function employs '<<-' operator which can be used to  
##    assign a value to an object in an environment that is different from 
##    the current environment, and consists of a list of multiple functions.

makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL
        set <- function(y) {      # Function sets the value of the matrix.
        x <<- y
        inv <<- NULL
        }
        get <- function() x      # Function gets the value of the matrix.
        setinv <- function(solve) inv <<- solve # Function sets the value 
                                                #     of the matrix inverse.
        getinv <- function() inv  # Function gets the value of the inverse
        list(set = set, get = get,
        setinv = setinv,
        getinv = getinv) # The list of functions is created.
  
}

## The second function, cacheSolve, computes the inverse of the special  
##    "matrix" returned by the first function makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), 
##    then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## To return a matrix that is the inverse of 'x'
  
        inv <- x$getinv()   # inv gets previously 'cached'inverse value, 
                            #   otherwise inv is assigned a NULL value
        if(!is.null(inv)) {           # If inv is not a NULL,
        message("getting cached data")
        return(inv)         # the previously cached inverse is returned; ELSE:
        }
        data <- x$get()               # the data (matrix) is obtained 
        inv <- solve(data, ...) # to calculate the matrix inverse afresh;
        x$setinv(inv)   # the resulting value of the inverse is set in the cache;
        inv       # and the newly calculated inverse is returned
}

