## Allows you to store a numeric matrix, set (or change) the value of the matrix, cache a value of a mean, or calculate the value
## of the mean if nothing is cached. 

## Allows input of a matrix and defines functions to set matrix, return the matrix, set a value for the matrix inverse (or "cache" it)
## and return the inverse. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y 
    inv <<- NULL 
  }
  get <- function() x
  setinv <- function(inverse) inv <<-inverse 
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Gets inverse from cache or, if nothing in cache, calculates inverse and caches that result. Returns result. 

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("gettingcacheddata")
    return(inv)
  }    
  data = x$get()
  inv=solve(data)
  x$setinv(inv)
  inv
  ## Return a matrix that is the inverse of 'x'
}


