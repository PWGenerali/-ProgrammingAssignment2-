## Creates a matrix and casches its inverse

## Creates a matrix, usable to cache the inverse

makeCacheMatrix <- function(x = matrix()) {
          x_inverse <- NULL
          set <- function(y) {
            x <<- y
            x_inverse <<- NULL
          }
          get <- function() x
          setinverse <- function(inverse) x_inverse <<- inverse
          getinverse <- function() x_inverse
          list(set = set, get = get,
               setinverse = setinverse,
               getinverse = getinverse)
}


## Calculates the inverse of a matrix or returns inverse, if already calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        x_inverse <- x$getinverse()
        if(!is.null(x_inverse)) {
        message("getting cached data")
        return(x_inverse)
        }
        data <- x$get()
        x_inverse <- solve(data, ...)
        x$setinverse(x_inverse)
        x_inverse
}
