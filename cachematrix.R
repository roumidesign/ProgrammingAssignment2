## Put comments here that give an overall description of what your
## functions do

## This function creates a special “matrix” object that can cache its inverse.
## 

makeCacheMatrix <- function(x = matrix()) {
        my_invers <- NULL
        set <- function(y) {
                x <<- y
                my_invers <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) my_invers <<- inverse
        getInverse <- function() my_invers
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## This function computes the inverse of the special “matrix” returned by makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        my_invers <- x$getInverse()
        if (!is.null(my_invers)) {
                message("getting cached data")
                return(my_invers)
        }
        data <- x$get()
        my_invers <- solve(data, ...)
        x$setInverse(my_invers)
        my_invers
}