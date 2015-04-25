## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly.
## The following two functions:
## a) create a special "matrix" object that can cache its inverse
## b) compute the inverse of the "matrix" object

## makeCacheMatrix: This function creates a special "matrix" object
## that can cache its inverse.
## List of makeCacheMatrix subfunctions and their description:
## "set" - sets the matrix
## "get" - gets the matrix
## "setinverse" - sets the inverse of the matrix
## "getinverse" - gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL # cached inverse matrix
    set <- function(y) {
        x <<- y         # set new matrix
        m <<- NULL      # clear cached inverse matrix
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## cacheSolve: This function computes the inverse of the special
## "matrix" returned by makeCacheMatrix above. If the inverse
## has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        # there was a cached inverse matrix so return it
        message("getting cached data")
        return(m)
    }
    # there was NO cached inverse matrix so calculate nad return it
    temp <- x$get()
    m <- solve(temp)
    x$setinverse(m)
    return(m)
}
