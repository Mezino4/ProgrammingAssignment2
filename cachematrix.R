## The function creates a special matrix object that can cache  
## the inverse of the special matrix for easy computation.

## This function creates a special matrix object that can cache
## the inverse

makeCacheMatrix <- function(x = matrix()) {
    x <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    set_invMatrix <- function(invMatrix) inv <<- invMatrix
    get_invMatrix <- function() inv
    list(set = set, get = get, set_invMatrix = set_invMatrix, get_invMatrix = get_invMatrix)
}


## This function computes the inverse of the matrix from the 
## above function

cacheSolve <- function(x, ...) {
    inv <- x$get_invMatrix()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$set_invMatrix(inv)
    inv
}
