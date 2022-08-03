## The purpose of these functions is to create a 'matrix' object that can access
## and cache the inverse of that matrix. 

## makecacheMatrix is a function that will create a list of functions to set the
## value of matrix, get the value of the matrix, set the value of the inverse, 
## get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x 
  
  setinverse <- function(inverse) inv <<- inverse
  
  getinverse <- function() inv
  
  list(set=set, 
       get=get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


## cacheSolve is a function that will compute the inverse of the special 
## 'matrix' returned by the function makecacheMatrix. If the inverse has already 
## been calculated (and the matrix is unchanged), then the cachesolve retrieves
## the inverse from the cache

cacheSolve <- function(x, ...) {
  
  inv <- x$getinverse()
  
  if(!is.null(inv)){
    message('getting cached data')
    return(inv)
  }
  
  data <- x$get()
  
  inv <- solve(data, ...)
  
  x$setinverse(inv)
  
  inv
}
