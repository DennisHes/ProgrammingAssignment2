## these functions cache the inverse of a matrix

## makeCacheMatrix creates a matrix object to chache the inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get<-function() x
    setMatrixInverse <- function(solve) m <<- solve
    getMatrixInverse <- function() m
    list(set=set, get=get, setMatrixInverse=setMatrixInverse, getMatrixInverse=getMatrixInverse)
}


## cacheSolve either computes the inverse of the matix or retrieve 
## the cached matrix assuming the matrix is inversible

cacheSolve <- function(x, ...) {
    ##retrieve matrix
    m <- x$getMatrixInverse()
    ##if the matrix is not NULL then return it
    if (!is.null(m)){
      return(m)
    }
    ##get the matrix
    inversedMatrix <- x$get()
    ##calculate the inverse
    m <- solve(inversedMatrix, ...)
    ##save the inverse
    x$setMatrixInverse(m)
    m
}
