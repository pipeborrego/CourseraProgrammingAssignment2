# CourseraProgrammingAssignment2 by pipeborregom@gmail.com

## These functions calculate the inverse of a given matrix and saves times by not
## calculating it if the inverse of the given matrix has already been calculate

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function()x
  setinv<-function(solve)inv<<-solve
  getinv<-function()inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## This function returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {   
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
