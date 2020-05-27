## Following functions are used to create a special "matrix" object 
##that stores the matrix and caches its inverse.



## The function, makeCacheMatrix creates a special matrix, which is
## really a list containing functions to :
## 1. set the value of the martix
## 2. get the value of the matrix
## 3. set the value of the matrix
## 4. get the value of the matrix

makeCacheMatrix <- function( x = matrix()){
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}



## The function, cacheSolve computes the inverse of the special matrix
## returned by makeCacheMatrix. If the inverse has already been calculated
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}