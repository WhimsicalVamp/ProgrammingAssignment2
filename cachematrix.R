## First function sets value of matrix
## second function acts as cache

## create matrix simillar to the numeric vector example

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  setmat <- function(y){
    x <<- y
    inv <<- NULL
  }
  getmat <- function() x
  setinv <- function (inverse) inv<-inverse 
  getinv <- function () inv
  list(setmat = setmat, getmat = getmat,
       setinv = setinv,
       getinv = getinv)
}


## Check for existing cache, else compute inverse and set it in cache

cacheSolve <- function(x, ...) {
  inv <- x$getinv
  if(!is.null(inv)){
    return(inv)
  }
  mat <- x$getmat()
  inv <- solve(mat)
  x$setinv(inv)
  inv
  ## Return a matrix that is the inverse of 'x'
}