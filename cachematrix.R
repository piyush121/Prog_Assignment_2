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


cacheSolve <- function(x, ...) {
    i<- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i<- solve(data, ...)
    x$setinv(i)
    i
}