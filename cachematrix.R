## These functions are designed to create a matrix that
## can cache its own inverse and either recall that
## inverse from the cache or calculate the inverse if 
## no cached object exists.

## The makeCacheMatrix function creates a special matrix
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

    minverse <- NULL
    
    ## set the matrix
    set <- function(y) {
        x <<- y
        minverse <<- NULL
    }
    
    ## return the matrix
    get <- function() x
    
    ## sets the inverse of the matrix
    setinverse <- function(inverse) minverse <<- inverse
    
    ## returns the inverse of the matrix
    getinverse <- function () minverse
    
    list(set = set, get = get, 
         setinverse = setinverse,
         getinverse = getinverse)
  
}


## The cacheSolve function computes the inverse of the
## special matrix returned by makeCacheMatrix.  If the 
## inverse has already been calculated (and the matrix
## has not changed), then cacheSolve returns the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 
    ## matrix 'x'

    ## attempt to recall a previously cached inverse
    minverse <- x$getinverse()
  
    ## Check to see if the inverse has already been
    ## calculated.
    if(!is.null(minverse)) {
        ## If it has, return the inverse from the cache.
        message("Getting cached data")
        return(minverse)
    }
  
    ## If it hasn't, calculate the inverse and cache
    ## it.

    ## Get matrix data
    matrixdata <- x$get()
    ## Calculate the inverse
    minverse <- solve(x$get(), ...)
    ## Cache
    x$setinverse(minverse)

    ## Return the inverse.
    return(minverse)
}
