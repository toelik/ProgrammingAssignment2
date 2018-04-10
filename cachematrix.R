## Cachematrix.R
## Data Science Coursera: Week 3
## toelik, 4/9/2018
## Cache the inverse of a matrix

## Create a special "matrix" object and cache its' inverse
## input to makeCacheMatrix is a matrix

makeCacheMatrix <- function(x = matrix()) {  
      i <- NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      get <- function() x
      setinv <- function(inv) i <<- inv
      getinv <- function() i
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}


## Compute the inverse of the matrix returned by makeCacheMatrix.
## If the inverse has already been calculated then retrieve the inverse from the cache
## Otherwise, compute the inverse using solve function

## input to cacheSolve is the output of makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      i <- x$getinv()
      if(!is.null(i)) {
            message("getting cached data")
            return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setinv(i)
      i
}
