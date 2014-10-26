## Here are a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
         m <- NULL
         set <- function(y) {
               x <<- y
               m <<- NULL
         }
         get <- function() {
              x
         }
         setCacheMatrix <- function(solve) {
              m <<- solve
         }
         getCacheMatrix <- function() {
              m
         }
         list(set = set, get = get, 
         setCacheMatrix = setCacheMatrix, 
         getCacheMatrix = getCacheMatrix)
}

## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve should retrieve 
##the inverse from the cache.

cacheSolveMatrix <- function(x, ...) {
        m <- x$getCacheMatrix()
        if(!is.null(m)) {
             message("getting cached data")
             return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setCacheMatrix(m)
        m
}

