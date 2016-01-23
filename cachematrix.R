## These functions provide the capability for caching the calcualted
## result of the inverse of a square matrix.  The calculation will only
## be performed once and the subsequent requests will return that value.

## makeCacheMatrix provides a wrapper with getter and setter methods for storing a matrix and an inverse value.
## There are no calculations performed here it is just a data structure with accessor functions.
## The input x is assumed to be a square invertible matrix.
##
## Usage:
##    > a <- makeCacheMatrix( matrix (rexp(200, rate=.1), 5,5 ) )
##
makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      
      set <- function(y) {
            x <<- y
            i <<- NULL
            
      }
      
      get <- function() {
            x
      }
      
      
      setinverse <- function(inverse) {
            i <<- inverse
      }
      
      getinverse <- function() {
            i
      }
      
      list(
            set = set, get = get ,setinverse = setinverse, getinverse = getinverse
      )
      
}


## cacheSolve takes the wrapped square matrix returned by makeCacheMatrix as an input.
## The first time it is called the inverse is calculated with the solve function and
## stored using the setinverse function of the input x.
## Subsequent calls will return the cached value from getinverse without recalculating.
##
## Usage:
##    > a <- makeCacheMatrix( matrix (rexp(200, rate=.1), 5,5 ) )
##    > cacheSolve(a)

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      i <- x$getinverse()
      
      if (!is.null(i)) {
            message("getting cached data")
            return(i)
      }
      
      data <- x$get()
      i <- solve(data,...)
      x$setinverse(i)
      
      i
      
}
