## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makeCacheMatrix takes a matrix as an input, and attaches to it a list of four attributes
# (or functionalities), $get(), $set(), $getinv(), $setinv(solve), to be used in the input
# for the next function cashSolve()
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function
# cacheSolve takes as an input, the output of makeCacheMatrix, uses its attributes
# to print out the cached inverse of the original matrix if the cache is available, or 
# computes the inverse for the first time, if the cache is not available, and 
# prints out the result.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
