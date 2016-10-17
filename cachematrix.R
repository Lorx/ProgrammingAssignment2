## The following 2 functions will compute the inverse of a given matrix 'x'
## and cache it or retrieve the cached inverse matrix if it was already computed

## makeCacheMatrix consists of 4 functions and createx a special "matrix" object
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function (y) {
                x <<- y
                s <<- NULL
        } 
        get <- function() x
        setsolve <- function(solve) s<<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
        
}


## cacheSolve first determines if the inverse matrix was already computed
## and searches for the cached inverse matrix. If there is no cached inverse matrix,
## cacheSolve computes and returns the inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
