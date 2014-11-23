## The function "makeCacheMatrix" creates a list containing a function to
##	1. "set" sets the value of the matrix
##	2. "get" gets the value of the matrix
##	3. "setInverse" sets the value of the inverse matrix
##	4. "getInverse" gets the value of the inverse matrix

makeCacheMatrix <- function(x = numeric()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- i
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## The function "cacheSolve" first checks to see if the 
## inverse matrix has already been calculated. If so, it
## gets the mean from the cache and skips the computation.
## Otherwise, it calculates the mean of the data and sets
## the value of the inverse matrix in the cache via the
## setInverse function.

cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}
