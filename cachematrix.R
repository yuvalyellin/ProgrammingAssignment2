## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly
## The following pair of functions is used to store and use the inverse of a matrix. 

## makecachematrix  creates an object that stores the 
## inverse of a matrix and fetches it : set is used to invalidate the cache, 
## setinverse and getinverse - to store and get the value

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Thie function returns the inverse of the matrix. 
## It tries to use a cached value, if none is present  - it caches the result of the 
## calculation

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
