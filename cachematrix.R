## Get the inverse of a matrix.  
## The inverse is retrieved if possible.
## If not, the inverse is calculated and stored for future retrieval

## makeCacheMatrix returns a list of functions to 
##   - get (retrieve the matrix)
##   - set (store the matrix)
##   - getInverse (return the currently stored inverse)
##   - setInverse (save the passed in value of the inverse)
## local variable i holds the current value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## cacheSolve is passed a list of functions, created by makeCacheMatrix.
## It calls getInverse to see if the inverse has already been calculated.
## If not, it calculates the inverse and then saves it via the setInverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}
  
