## This program gives a pair of functions that cache the inverse matrix.
## Function 1 - makeCacheMatrix: Creates a special matrix object  
## Function 2 - cacheSolve: Computes the inverse of the matrix returned by makeCacheMatrix 
## Assume that the matrix supplied is always invertible 

## Function 1: Creates a special matrix which is a list of functions (a) - (d)
## (a) Function "set": Defines to set the matrix 'x' with assigning 'y'
##     and reset the matrix 'inv'
## (b) Function "get": Gets the matrix 'x' retrieved from the environment of "makeCacheMatrix"
## (c) Function "setinvmat": Defines the matrix 'inv' and 
##     assigns the input argument to the value of 'inv' in the environment of "makeCacheMatrix" 
## (d) Function "getinvmat": Returns the inverse matrix 'inv'
## Then this function returns the matrix contating all of the functions above

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinvmat <- function(invMat) inv <<- invMat
        getinvmat <- function() inv
        list(set = set, get = get, 
             setinvmat = setinvmat, getinvmat = getinvmat)
}

## Function 2: Calculates and returns the inverse of the matrix returned by "makeCacheMatrix"  
##             Uses the "solve" function in R to compute the inverse of matrix

cacheSolve <- function(x, ...) {
        inv <- x$getinvmat()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)  
        x$setinvmat(inv)
        inv
} 