## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The makeCacheMatrix creates a special matrix object that has the ability to cache 
## its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInv <- function(inverse) inv <<- inverse
        getInv <- function() inv
        list(set = set, get = get,
             setInv = setInv, 
             getInv = getInv)
}


## Write a short comment describing this function
## This function uses the makeCacheMatrix function to compute the inverse of the the 
## special "matrix" that was computed by the above funciton.  Assuming that the inverse 
## has been calculated by the makeCacheMatrix & that no changes have been to the matrix, 
## then this function will retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        matrx <- x$get()
        inv <- solve(matrx, ...)
        x$setInv(inv)
        inv
}
