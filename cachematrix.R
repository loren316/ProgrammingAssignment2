##makeCacheMatrix is a function that returns a list of 
##  functions: set, get, setinv, and getinv.
##The default value of argument x is a 1x1 matrix with
##  value 1.
makeCacheMatrix <- function(x = matrix(c(1),nrow=1,ncol=1)) {
  myinv <- NULL
  ## The function "set" changes the value of x to y and 
  ##  resets the inverse value.
  set <- function(y) {
    x <<- y
    myinv <<- NULL
  }
  ## The function "get" just retrieves the value of the 
  ##  matrix stored in x.
  get <- function() x
  ## The function "setinv" sets the value of the "inverse"
  ##  to an input value specified by argument savedInv.
  setinv <- function(savedInv) myinv <<- savedInv
  ## The function "getinv" retrieves the inverse value.
  getinv <- function() myinv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## The cacheSolve function evaluates the inverse of matrix x
##  (assumed to be an invertible matrix - this condition is 
##  not checked), using cached data if available.  If no 
##  cached data is available, then it goes ahead and 
##  calculates the matrix inverse.
cacheSolve <- function(x) {
  
  ## First check for cached data.  If such data exists,
  ##  return it and exit the function.
  myinv <- x$getinv()
  if(!is.null(myinv)) {
    message("getting cached data")
    return(myinv)
  }
  
  ## If cached data was not found, go ahead and calculate
  ##  the matrix inverse.
  data <- x$get()
  myinv <- solve(data)
  x$setinv(myinv)
  myinv
}