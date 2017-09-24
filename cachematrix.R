## Create a cashed matrix to pull into other environments.

## This function creates the cashed inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m = NULL
  ##set value of matrix
  set = function(y) {
    x <<- y
    m <<- NULL
  }
  ##get value of matrix
  get = function() x
  ##set inverse
  setinverse = function(solve) m <<- solve
  ##get inverse
  getinverse = function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function checks to see if a cached inverse matrix exists.
## If so, bring it in
## If not, calculate the inverse matrix

cacheSolve <- function(x, ...) {
  m = x$getinverse()
  ##if inverse already calc'd, bring it in here
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ##if not already calc'd, calc it
  data = x$get()
  m = solve(data, ...)
  x$setinverse(m)
  return(m)
}
