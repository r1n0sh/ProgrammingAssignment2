##        In Linear algebra, a square matrix is invertible if there exists 
## another matrix such that product of the two matrices would result in 
## Identical Matrix. 
##        The main objective of functions below is to cache the inverse of a matrix
##

##        The function "makeCacheMatrix" creates a special "matrix" object that  
## can cache its inverse.

makeCacheMatrix <- function(x = matrix()) { 
    inv <- NULL
    set <- function(y) {     
        x <<- y
        inv <<- NULL
    }
    get <- function() x 
    setinverse <- function(inv) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
