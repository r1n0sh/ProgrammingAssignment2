##        In Linear algebra, a square matrix is invertible if there exists 
## another matrix such that product of the two matrices would result in 
## Identical Matrix. 
##        The main objective of functions below is to cache the inverse of a matrix
##

##        The function "makeCacheMatrix" creates a special "matrix" object that  
## can cache its inverse. This function consists of a list containing a function to
## 1. set the elements of the matrix
## 2. get the elements of the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) { 
    inv <- NULL
    set <- function(y) {            ## set the elements of the matrix
        x <<- y
        inv <<- NULL
    }
    get <- function() x             ## get the elements of the matrix
    
    setinverse <- function(inverse) inv <<- inverse ## set the inverse of the matrix
    
    getinverse <- function() inv    ## get the inverse of the matrix
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


##           The function "cacheSolve" calculates the inverse of the special "Matrix" 
## created with the above function. However, it first checks to see if the inverse
## has already been calculated. If so, it gets the inverse from the cache and skips  
## the computation. Otherwise, it calculates the inverse of the matrix and sets the  
## inverse matrix in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {                 ## Checks whether the inverse has been computed already
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)             ## Computes the inverse of given matrix
    
    x$setinverse(inv)                   ## Sets the inverse of matrix in the cache
    inv
}
