## The two following functions are used to cache matrix so that we dont have to compute them
## repeatly and save memmory for other tasks

## This function builds a list of function to be appllied for the input matrix

makeCacheMatrix <- function(x = matrix()){
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


## This function returns the inverse matrix if it has already been computed or compute the inverse matrix and
## return it

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
