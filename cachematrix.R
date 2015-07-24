# As matrix inversion is a costly operation, particularly for large
# matrices, the functions makeCacheMatrix and cacheSolve will 
# cache the result of the inversion operation and use this cached value
# instead of recalculating the inverse, if the original matrix has not changed

# function makeCacheMatrix
# Creates a CacheMatrix 'object' which caches the matrix to be inversed 
# as well as the result of matrix inversion. 
# In reality, it is a list of get/set functions for the matrix 
# and its inverse
#
# Args:
#   x: Invertible matrix to be inversed. Defaults to 1x1 matrix with NA
#
# Returns:
#   A list of functions to get/set the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {  
    cachedInverse <- NULL  # Initialize cache for result of matrix inversion
    
    ## define the set, get functions for the matrix to be operated on
    
    # set function for matrix to be operated on
    setMatrix <- function(y) {    
        x <<- y    # store the passed in matrix for later operation
        cachedInverse <<- NULL    # reset the cached result of the inversion
    }
    
    # get function for matrix to be operated on
    getMatrix <- function() x    
    
    
    # define the set, get functions for the result of the matrix inversion
    
    # set function for caching the result of matrix inversion
    setInverse <- function(inverse) cachedInverse <<- inverse
    
    # get function to return the cached result of matrix inversion
    getInverse <- function() cachedInverse
    
    
    # return a list of the get and set functions 
    # for the matrix and its inverse
    list(setMatrix = setMatrix, 
         getMatrix = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse)
}


# function cacheSolve
# Returns the inverse of the matrix stored in the CacheMatrix 'object' passed in
# If the inverse has already been calculated (and cached),
# it returns the cached inverse instead of re-calculating it.
# Note: a CacheMatrix is created using the makeCacheMatrix function
#
# Args:
#   x: a CacheMatrix storing an invertible matrix
#   ...: Optional arguments for solve()
#
# Returns:
#   The matrix that is the inverse of the matrix stored in CacheMatrix 'x'
cacheSolve <- function(x, ...) {
    
    # get the cached inverse and return it if it is not NULL
    # note: the cached inverse is not NULL if it has been calculated already
    inverse <- x$getInverse()  # get cached inverse
    if(!is.null(inverse)) {  # check if inverse has been calculated already
        message("getting cached inverse")
        return(inverse)
    }
    
    # inverse has not been calculated (cached) before
    # get the matrix, calculate the inverse, cache it and then return it
    m <- x$getMatrix()  # get the matrix to be inversed
    inverse <- solve(m, ...)  # solve for the inverse
    x$setInverse(inverse)  # cache the inverse
    inverse # return the inverse
}

# Example Usage:
# > x <- matrix(c(1,1,1,3,4,3,3,3,4),3,3)
# > cm <- makeCacheMatrix(x)
# > cacheSolve(cm)
# [,1] [,2] [,3]
# [1,]    7   -3   -3
# [2,]   -1    1    0
# [3,]   -1    0    1
# > cacheSolve(cm)
# getting cached inverse
# [,1] [,2] [,3]
# [1,]    7   -3   -3
# [2,]   -1    1    0
# [3,]   -1    0    1