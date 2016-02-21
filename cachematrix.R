## This file contains two functions that calculate and cache the result of a matrix inversion.
## Caching the result of a matrix inversion is beneficial as the computation is costly and the 
## result of the matrix inversion may be reused many times when analysing the data in the matrix.
##
## A typical session using these two funtions might look like this:
##
## source("cachematrix.R") # load this function
## testmat <- matrix(rnorm(16),nrow=4,ncol=4) # create some test data
## cm <- makeCacheMatrix(testmat) # create the cache
## cm$get() # get the matrix to check its value
## cm$getinv() # try to get the inverse. It should be NULL as it have not been calculated yet
## inv <- cacheSolve(cm) # use the companion function to calculate and cache the inverse. 
## checkinv <- cm$getinv() # use the get method.
## identical(inv,checkinv)


## This function provides storage to hold a matrix and its inverse.
## It contains functions to set and get the matrix and the inverse.
## This funcion is similar to a java class where the properties are 
##   x - the matrix
##   invx - the inverse
## and the methods are
##   set() - set a value for the matrix and initialise the inverse to NULL
##   get() - get the current value of the matrix
##   setinv() - set the value for the inverse
##   getinv() - get the cached inverse (or NULL if it has never been set.
## The input parameters to the function are
##   x - the matrix to be cached
## The output from the function is a list containing the four methods (functions) listed above.
##
makeCacheMatrix <- function(x = matrix()) {
     # Initialise the inverse to NULL in case getinv() is called before set().
    invx <- NULL
    # Create the set() function that caches the original matrix and initialises the cached inverse to NULL
    set <- function(y) {
        x <<- y
        invx <<- NULL
    }
    # Create the get() function that returns the original matrix
    get <- function() x
    # Create the setinv() function the caches the inverse. 
		# Note, the inverse in NOT calculated in this function. It is only cached here.
    setinv <- function(inv) invx <<- inv
    # Create the getinv() function that returns the cached inverse or NULL if the inverse has not been cached.
    getinv <- function() invx
    # Return a list of function (pointers)
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function uses the functions from makeCacheMatrix to cache a matrix and its inverse.
## The matrix inverse is calculated only once in this function.
## Input parameters
##   cx - a cached matrix and inverse object represented as a list of functions created by calling makeCacheMatrix(x)
## Ouput parameters
##   invx - the inverse of the matrix used to create x (i.e. x used in makeCacheMatrix(x))
##          return from the cache if available
## Side effects                            
##   calcuate and save the inverse of z in x if not already calculated 
cacheSolve <- function(cx, ...) {
	  # Get the inverse from the cache
    invx <- cx$getinv()
    # If the inverse is not NULL, it has been calculated already, so return it.
    if (!is.null(invx)) {
        message("getting cached inverse")
        # Return the inverse recovered from the cache.
        return(invx)
    }
    # Only get here if invx is NULL, i.e. not calculated and cached yet, so we need to calculated it now.
    # Recover the matrix used to create cx.
    data <- cx$get()
    # Calculate the inverse.
    invx <- solve(data, ...)
    # Cache the inverse in cx.
    cx$setinv(invx)
    # Return the inverse we have just calcuated.
    invx
}
