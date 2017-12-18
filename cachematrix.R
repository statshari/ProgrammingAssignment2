## makeCacheMatrix creates a "matrix" object that can cache its inverse.
##cacheSolve computes the inverse of the "matrix" returned by makeCacheMatrix. 
##If the inverse has already been calculated (and the matrix has not changed), then 
##the cachesolve retrieves the inverse from the cache.

## makeCacheMatrix creates a list ("matrix") function that
##sets the value of the matrix, get the value of the matrix, set the value of the inverse and get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## cacheSolvefunction calculates the inverse of the "matrix" above. 
##It first checks to see if the inverse is already calculated and in that case it retrieves the inverse from 
##the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets 
##the value of the inverse in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m        
}
