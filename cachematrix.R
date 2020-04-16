## These functions calculate the inverse of a matrix, cache it, 
## and then retrieve it without another calculation

## Cache the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinv <- function(solve) inv <<- solve
      getinv <- function() inv
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}


## Retrieve the inverse of the matrix if exists in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv.local <- x$getinv()
      if(!is.null(inv.local)) {
            message("getting cached data")
            return(inv.local)
      }
      data <- x$get()
      inv.local.calc <- solve(data, ...)
      x$setinv(inv.local.calc)
      inv.local.calc
}
