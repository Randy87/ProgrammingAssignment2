## The two functions are for creating a matrix and solving it. If the solving is already done, it just returns the value from the cache instead of a repeated calculation.

## This function creates the matrix and sets m = NULL.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL                               ## sets an empty m
  set <- function(y) {                    ## saves x in y and set m to NULL
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,               ##returns a list with 4 items
       setsolve = setsolve,
       getsolve = getsolve)

}


## This function tests m for a matrix that is already solved. If it is = NULL, is solves the matrix. if there is a solved matrix saved in m, it just returns m from the cache.

cacheSolve <- function(x, ...) {
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
