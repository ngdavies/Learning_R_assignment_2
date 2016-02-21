## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	  # Initialise the inverse to NULL in case getinv() is called before set()
    invx <- NULL
    # Create the function that caches the original matrix and initialised the cached inverse to NULL
    set <- function(y) {
        x <<- y
        invx <<- NULL
    }
    # Create the function that returns the original matrix
		get <- function() x
    # Create the function the caches the inverse (that is calculted outside makeCacheMatrix
    setinv <- function(inv) invx <<- inv
    # Create the function that returns the cached inverse or NULL if the inverse has not been cached
    getinv <- function() invx
    # Return a list of function (pointers)
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invx <- x$getinv()
    if (!is.null(invx)) {
        message("getting cached inverse")
        return(invx)
    }
    data <- x$get()
    invx <- solve(data, ...)
    x$setinv(invx)
    invx
}


#     # This is a kind of R class with properties
#     #  x - the original vector of data
#     #  m - the (possibly) cached mean of the vector x
#     # and methods
#     #  set     - set the property x
#     #  get     - get the property x
#     #  setmean - set (cache) the mean of the vector
#     #  getmean - get the mean of the vector.
#     # Note, this object does not calculate the mean, it just caches the value it is given
#     # Whenever the object is reused with new input data (i.e. when set(x) is called, 
#     #  the cached mean is reset to NULL until it is recalculated
#     makeVector <- function(x = matrix()) {
#         m <- NULL
#         set <- function(y) {
#             x <<- y
#             m <<- NULL
#         }
#         get <- function() x
#         setmean <- function(mean) m <<- mean
#         getmean <- function(m) m
#         list(set=set, get=get, setmean=setmean, getmean=getmean)
#     }
#     
#     # This function returns the mean of the cached mean object
#     # If the mean has never been calculated, it calculates and caches it
#     # If the mean has already be calculated and cached, it returns the cached value
#     # The works if the users calls makeVector to create a mean caching object
#     # that is passed into this function.
#     cachemean <- function(x, ...) {
#         m <- x$getmean()
#         if (!is.null(m)) {
#             message("getting cached data")
#             # Return the calculated mean recovered from the cache
#             return(m)
#         } else {
#     				# The cache object indicates that the cached mean has not be calculated and cached
#     				# Recover the data from the cache object
#     				data <- x$get()
#     				# Calcuate the mean, and does ... pass on any extra parameters?
#     				m <- mean(data, ...)
#     				# Cache the calculated mean.
#     				x$setmean(m)
#             # Return the newly calculated mean.
#     				m
#     		}
#     }
#     # Improvements I would make:
#     #   - add another method hasCachedValue() that returns TRUE or FALSE
#     #   - or, calculate the mean on creating the class so the cached value is immediately available
#     #   (This would remove the need for the first function)
#     #   - or, if user calls for getmean and it is null, calculate it.
#         
#     ##     
#     ## cachemean <- function(x, ...) {
#     ##     m <- x$getmean()
#     ##     if (!is.null(m)) {
#     ##         message("getting cached data")
#     ##         return(m)
#     ##     } 
#     ##   data <- x$get()
#     ##     m <- mean(data, ...)
#     ##     x$setmean(m)
#     ##     m
#     ## }
#     ##     
#     
#     
