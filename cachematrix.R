## These functions calculate inverse of a matrix and if the matrix has already been 
## calculated then it just returns the cached value for faster computation

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL          ## sets value to NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  
  l<-list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
  
  l     ##    returns the list that consist of 4 elements
}



## Write a short comment describing this function

CacheSolve <- function(x=matrix(), ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()  
  m <- solve(data, ...)     ##   calculates the inverse of a matrix using solve()
  x$setinverse(m)
  m
}