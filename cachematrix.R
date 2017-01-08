##I create a pair of functions that cache the inverse of a matrix.
##First of all I create the following function of special "matrix" object 
##that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  ##This matrix should be a square matrix in order to be invertible but we assume
  ##that the matrix supplied is always invertible.
  inv <- NULL
  ##Set the matrix.
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  ##Get the matrix.
  get <- function(){
    x
  }
  ##Set the inverse of the matrix.
  setinverse <- function(inverse){
    inv<<-inverse
  }
  ##Get the inverse of the matrix.
  getinverse <- function(){
    inv
  }
  ##Create a list of the above methods.
  list(set = set,get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##Secondly I create a function which is called cacheSolve and 
##computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed),
##then the cachesolve should retrieve the inverse from the CACHE.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  ##In case of the inverse has already been calculated then the cacheSolve
  ##should retrieve the inverse from the cache.
  if(!is.null(inv)){
    message("Getting cached data")
    return(inv)
  }
  ##Get the matrix.
  data <- x$get()
  ##Solve function calculates the inverse using matrix multiplication.
  inv <- solve(data)
  ##Set the inverse.
  x$setinverse(inv)
  ##Return the inverse matrix.
  inv
  }
