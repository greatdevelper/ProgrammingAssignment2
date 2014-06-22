## The two functions below are used to create a special object that stores a matrix and cache's its inverse

## The makeCacheMatrix creates a list which contains functions to 
## 1. set the value of a matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of a matrix
## 4. get the value of the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inverse) i<<- inverse
  
  getInverse <- function() i
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}


## The cacheSolve function calcualtes the inverse of a matrix created by the above function. However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. Otherwise it checks to see if it is the same vector and calculates the inverse and sets the value of the inverse in the cache via the setInverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  
  data <- x$get()
  i <- solve(data,...)
  x$setInverse(i)
  i
}
