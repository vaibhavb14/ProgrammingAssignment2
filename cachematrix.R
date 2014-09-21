## The functions take any invertible matrix and check in 
## the cache if it is already inverted. If so the value 
## of the inverse is fetched, else the cache is updated
## with the calculated inverse and the value is stored
## in the cache.

## This function creates a matrix and stores its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)    
}


## This function checks for the inverse in the cache and 
## displays it if it exists, else calculates the inverse 
## and updates the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}