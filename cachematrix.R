## These two functions are able to cache the inverse of matrix
## in order to avoid repetitive calculation

## makeCacheMatrix function creates a special "matrix" that can cache its inverse
## the output is a list of functions that will be used as arguments in cacheSolve

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function(){
    x
  }
  setinv <- function(solve) {
    inv <<- solve
  }
  getinv <- function (){
    inv
  }
  list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## cacheSolve takes arguments from makeCacheMatrix and either returns the cached inverse
## or calculate the inverse 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
    }
  data <- x$get()
  inv <- solve(data,...)
  x$setinv(inv)
  inv
}
