## The following two functions can be used to create a
## special object that stores a matrix and caches its inverse.

## The `makeCacheMatrix` function creates a special "matrix" object, which
## contains functions to set and return (`get`) the matrix, and to set and
## and return its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The `cacheSolve` function calculates the inverse of the special "matrix"
## created with the `makeCacheMatrix`. However, it first checks to see if the
## inverse has already been calculated, in which case it returns (`get`s) the 
## inverse from the cache and skips the computation. Otherwise, it calculates 
## the inverse of the matrix and sets the value of the inverse in the cache 
## via the `setinverse` function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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