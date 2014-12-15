## Caching the Inverse of a Matrix

## Calculation of the inverse of a matrix can be potentially
## time consuming computation, therefore it make sense to cache it.
## Below are two functions that are used to create a special
## object that stores a numeric matrix and cache's its inversion.

## makeCacheMatrix is a "construction" function, it creates a special
## "matrix" object, which is really a list containing a function to:
##  - set/get the value of the matrix,
##  - set/get the value of the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
            x <<- y
            s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated then
## the cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        ## Check, if we calculated inverse already, then return it.
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        ## if not, calculate the inverse
        s <- solve(data, ...)
        ##  and cache it
        x$setsolve(s)
        s
}

## console listing:
## x <- matrix(c(2, 4, 3, 5),nrow=2,ncol=2)
## a <- makeCacheMatrix(x)
## cacheSolve(a)
