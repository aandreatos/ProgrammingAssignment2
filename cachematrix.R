## Programming Assignment 2: Lexical Scoping 
## This R script contains two functions: makeCacheMatrix and cacheSolve
## makeCacheMatrix takes a regular matrix and returns a set of 4 functions  
## cacheSolve: if inverse exists in cache, this function returns it; else, calculates it 
## In this Assignment we use the <<- operator which can be used to assign a value 
## to an object in a (global) environment that is out of the current function environment.  

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

###################################################
# Usage example:
# x1 <- matrix(rnorm(100), 10, 10) #create 10x10 test matrix 
# m1<- makeCacheMatrix(x1) # call makeCacheMatrix
# cacheinv(m1) # first call of  cacheinv; eval. inv &store it
# cacheinv(m1) # next calls retrieve inverse from cache
###################################################


