makeCacheMatrix <- function(matrix = matrix()) {
  i<- NULL
  set <- function(y) {
    matrix <<- y
    i<<- NULL
  }
  get <- function() matrix
  setinv <- function(matrix) i<<- solve(matrix)
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


cachemean <- function(x, ...) {
    i<- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    i<- mean(data, ...)
    x$setmean(m)
    m
}