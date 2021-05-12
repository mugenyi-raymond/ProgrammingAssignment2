## Put comments here that give an overall description of what your
## functions do

## There are two functions;makeCacheMatrix and cacheSolve.
## makeCacheMatrix  creates a special "matrix" object that can cache its inverse
## cacheSolve  computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## Write a short comment describing this function
##This function consists of set,get,setinverse,getinverse

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function(){x}
  setinverse <- function(inverse){inv <<- inverse}
  getinverse <- function(){inv}
  list(set = set , get = get, setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
##This function is used to get cached data

cacheSolve <- function(x, ...) {
        cacheSolve <- function(x,...){
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat,...)
  x$setinverse(inv)
  inv
}
        # Return a matrix that is the inverse of 'x'
}
