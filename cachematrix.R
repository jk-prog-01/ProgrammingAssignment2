## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix
## creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                m <<- y
                inv <<- NULL
        }
        get <- function() m
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve
## computes the inverse of a "matrix" object created by makeCacheMatrix
## if previously computed, returns cached value

cacheSolve <- function(x, ...) {
	  inv <- x$getinv()
 	  if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
