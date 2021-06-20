## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  #define the following 4 functions
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  
  #return the following list in order to pass it into cacheSolve
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  #get inverse from cache
  inv <- x$getinv()
  
  #if the inverse from cache exists (is not null)
  if(!is.null(inv)) {
    message("getting cached data")
    #return the inverse from cache
    return(inv)
  }
  
  #otherwise calculate the inverse
  data <- x$get()
  inv <- solve(data, ...)
  
  #set the new calculated inverse into cache
  x$setinv(inv) 
  
  inv 
}
