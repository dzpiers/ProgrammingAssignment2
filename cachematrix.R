## ProgrammingAssignment2

## This function creates a "matrix" in that it makes a list containing functions
## to set and get the value of the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function first checks to see if the inverse of the specified matrix has
## been stored. If so it skips the calculation, otherwise it calculates the
## inverse itself

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
## Return a matrix that is the inverse of 'x'

## Test for yourself here

A <- matrix(c(1,2,3,4),2,2)
A1 <- makeCacheMatrix(A)
cacheSolve(A1) ## Run this line twice to get the message
