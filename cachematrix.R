# Following function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(matrix = matrix()) {
      i<- NULL
#set the value of matrix
      set <- function(y) {
        matrix <<- y
        i<<- NULL
      }
#Get the value of matrix
      get <- function() matrix
      
#Set inverse of the matrix
      setinv <- function(matrix) i<<- matrix
     
#Get inverse of the matrix
       getinv <- function() i
#Return a list containing all the above functions
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
      
}

#Following function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    i<- x$getinv()
#Check if the inverse is already calculated.
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
#Calculate the inverse if it is not present in the cache and
#set the inverse in the cache.
    data <- x$get()
    i<- solve(data, ...)
    x$setinv(i)
    i
}