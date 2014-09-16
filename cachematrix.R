## These functions provide the ability to cache the inverse of a matrix in order to retrieve it
## without recalculation. makeCacheMatrix initializes the matrix and defines the functions for
## setting and retrieving the matrix and its inverse. cacheSolve returns the calculated (cached) inverse
## is such exists, and calulates it otherwise and stores in the cache.
## Example use:
## > y <- matrix(c(1, 1, 3 , 2, 2, 2, 3, 1, 1), nrow = 3, ncol = 3) # example invertible matrix
## > z <- makeCacheMatrix(y) # creates the matrix object
## > z$get() # prints the matrix
## > cacheSolve(z) # calculates and prints the inverse
## > z$getinverse() # prints the inverse
## > cacheSolve(z) # print the inverse, this time from cache "getting cached data"


## This function creates a makeCacheMatrix object and defines the functions it provides. 
## The function set sets the matrix x and its inverser inv in the global environment 

makeCacheMatrix <- function(x = matrix()) {
    # Reset the matrix inverse inv in the function execution environment 
    inv <- NULL

    # Function sets the matrix x in the global environemtn and
    # resets the matrix inverse inv in the global environment
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # Function returns the matrix x
    get <- function() x
    
    # Function sets both inv in the function execution environment and inv in the global enviroment
    setinverse <- function(inv) inv <<- inv
    
    # Function returns the matrix inverse
    getinverse <- function() inv
    
    # Define the list of functions the matrix object offers
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function checks whether an inverse of the matrix passed is already calculated
## If the inverse is not caluculated, i.e. the matrix object returns NULL as the inverse, 
## the inverse inv is calculated and set in the matrix x object

cacheSolve <- function(x, ...) {

    ## Return a matrix that is the inverse of 'x'

    # Set inv in the execution environment to the inverse value returned by the matrix object, 
    # which in turn is the inv value in the global environment
    inv <- x$getinverse()
    
    # Return inv from cache if it already calculated, i.e. not NULL
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    # If there is no pre calculated value for inv, i.e. it is NULL,
    # we calculate the inverse of the matrix x using a proxy variable data and store it in inv
    data <- x$get()
    inv <- solve(data, ...)
    
    # Call the matrix x's setinverse function to set the newly calculated matrix inverse
    x$setinverse(inv)
    
    # Retunr the matrix inverse
    inv
}