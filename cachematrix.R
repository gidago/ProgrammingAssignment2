#cachematrix.R
## 
## 

## Function creates a matrix object that can cache its inverse.
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

## This function computes the inverse of the matrix returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

## TEST
## From: https://class.coursera.org/rprog-013/forum/thread?thread_id=127
# 1.  m <- matrix(c(-1, -2, 1, 1), 2,2)
# 2.  x <- makeCacheMatrix(m)
# 3.  x$get()
# 4.  inv <- cacheSolve(x)
# 5.  inv
# 6.  inv <- cacheSolve(x)   ## Must emit message: getting cached data
