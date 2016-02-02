## Put comments here that give an overall description of what your
## functions do


# makeCacheMatrix takes a regular matrix and returns a set of 4 functions  
# which allow the time-consuming inverse calculation to be cached
makeCacheMatrix <- function(x = numeric()) {
# create object with 4 functions: set, get, setinv, getinv 
        inverse <- NULL # define a variable to hold cached inverse
			# and set the placeholder to NULL 
        set <- function(y) { # define a function 
                x <<- y # to set the matrix x to a new matrix y and
                inverse <<- NULL # reset the inverse to NULL
        }
        get <- function() x  # return the matrix  x
        setinv <- function(inv) inverse <<- inv 
		# set the inverse to  inv
        getinv <- function() inverse # Get the cached inverse value
	   # Return the  set of 4 functions
        list(set = set, get = get,
             setinv = setinv, getinv = getinv)
} #END_makeCacheMatrix 


cacheSolve <- function(x) {
# cacheSolve: if inverse exists in cache, return it; else calculate it 
        inverse <- x$getinv()
        if(!is.null(inverse)) { #  get cached inverse
                message("getting cached data") # print info message
                return(inverse)
        } #end_if 
# Otherwise, get the matrix and calculate the inverse
        data <- x$get()  # get the matrix
        inverse <- solve(data) # calculate inverse
        x$setinv(inverse)  # save result in the cache
        inverse  # return inverse  
} # END_cacheSolve 


