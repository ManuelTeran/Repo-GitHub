

##I took as a reference the makeVector function and customize to
##take as input a matrix invertible, changing the
##function mean by solve


makeCaheMatrix <- function(x = numeric()) {
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


##I took as a reference the cachemean function and customize to
##take as input the result of the function makeCahceMatrix to
##calculate the matrix inversa by the functionsolve if it were 
##not calculated in cache.


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
