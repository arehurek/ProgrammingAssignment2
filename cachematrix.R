## ================================================================
## HEADER
## ================================================================
## Following two functions can be easily used to calculate inverse 
## of a square invertible matrix
##      - "efficiency" feature being the caching of the inverse
##        while repeatedly feeding identical input matrix


## ================================================================
## Example of application:
## ================================================================
## abc <- makeCacheMatrix(matrix(1:4,2,2))
## cacheSolve(abc)


## ================================================================
## "makeCacheMatrix"
## ================================================================
## Creates vector of all necessary functions, which are being later
## called by the "cacheSolve" function
makeCacheMatrix <- function(x = matrix()) {
        Inv <- NULL
        set <- function(y) {
                x <<- y
                Inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) Inv <<- inverse
        getinverse <- function() Inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## ================================================================
## "cacheSolve"
## ================================================================
## Returns inverse of a matrix, either by retrieving it from cache 
## (if available) or by applying the built-in "solve" function
cacheSolve <- function(x, ...) {
        Inv <- x$getinverse()
        if(!is.null(Inv)) {
                message("getting cached data")
                return(Inv)
        }
        data <- x$get()
        Inv <- solve(data)
        x$setinverse(Inv)
        Inv
}
