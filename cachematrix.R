# Leif Wilm 
# Created: Wednesday, November 25th, 2020
# R Programming Course on Coursera by R.G Peng and associates
# Week 3 Assignment 2
#-----------------------------------------------------------------
#
# The following code calculates an inverse of a matrix (given that the matrix is inversable)
# and caches the inverse. If the inverse is already cached, then the code skips the
# calculation and retrieves the cache.


## The first function, makeCacheMatrix, creates a special 'Matrix' object that
##  can cache its inverse. Or more accurately, a list containing functions to 
##      1) Set the value of the matrix
##      2) Get the value of the matrix
##      3) Set the value of the inverse
##      4) Get the value of the inverse
##      
makeCacheMatrix <- function(x = matrix()) {
        # resets the i value to NULL
        i <- NULL
        # caches the matrix
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <-  function() x
        
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        # creates and returns the list of the four functions
        list( set = set, get = get,
              setinverse = setinverse,
              getinverse = getinverse)
}


## The following function takes a matrix, and checks to see if it's inverse is already cached. 
## If so, the function retrieves the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the given matrix and sets the inverse in the cache 
## via the setinverse function. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # Calls upon the getinverse function from makeCacheMatrix
        i <- x$getinverse
        # If the cached inverse is present, retrieves the inverse and returns it
        # exits out of the function, and in so does not caculate the inverse of the matrix
        if(!is.null(i)){
                message("Retrieving cached data...")
                message("Data obtained")
                return(i)
        }
        # if not found, the code below retrieves the matrix data
        data <- x$get()
        # calculates the inverse
        i <- Inverse(data, ...)
        # Caches the inverse
        x$setinverse(i)
        # and returns the inverse
        i
}
