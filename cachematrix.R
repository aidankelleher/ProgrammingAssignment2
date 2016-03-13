## These functions will allow the user to save an invertible matrix
## and cache the inverse of the matrix

## to generate a random invertible matrix
## mat<-matrix(rnorm(n*n), ncol=n, nrow=n)
## where n is an integer

## This function generates the cache functions.

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


## this function will solve an invertible matrix, or recall the cache if the
## matrix has already been solved.

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
