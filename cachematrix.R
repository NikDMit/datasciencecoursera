## These functions cache the inverse of x to save it having to be calculated 
## repeatedly 

## makeCacheMatrix creates a special matrix containing a number of functions 
## to ultimately chache its inverse

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cashsolve will check whether the inverse of x has been cached and return it.
## if the cache is empty the inverse will be calculated and cached

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached inverse of data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)  ##calculates and returns inversed data where has not been cached
  x$setinverse(m)
  m  
}
  
        
}
