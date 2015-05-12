## Matrix inversion is usually a costly computation and there may be
## some benefit to caching the inverse of a matrix rather than computing
## it repeatedly, these two functions cache the inverse of a matrix.

## First function creates a special "vector", which is really a list
## containing a function to:
## 1.set the value of the vector
## 2.get the value of the vector
## 3.set the inverse matrix
## 4.get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Function checks to see if the inverse matrix has been created,
## if so it gets it from the cache. If not the inverse is created and
## writen to cache.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}