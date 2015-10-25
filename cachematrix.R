## functions makeCahceMatrix and cacheSolve cache inverse of a matrix
## so there is no need to compute it repeatedly

## makeCahceMatrix creates matrix object wich cashes inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv){inverse <<- inv}
  getinverse <- function() inverse
  list(set=set, get=get, 
       setinverse=setinverse, 
       getinverse=getinverse)
}


## cacheSolve takes object created by makeChacheMatrix 
## and computes inverse for it if inverse hasn't been calculated yet.
## retrieves inverse from cache otherwise.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)){
    return(inverse)
  }
  data <-x$get()
  inverse <- solve(x$get())
  x$setinverse(inverse)
  inverse
}
