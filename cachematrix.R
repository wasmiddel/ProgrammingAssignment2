## The functions in this file create a cache matrix and return the inverse of the given 
## vector. The second function checks if the cache exists, otherwise it creates a new one

## create the matrix, get and set the values and get the inverse
makeCacheMatrix <- function(x = matrix()) {

  # set the value of the matrix
  m <- NULL
  set <- function(y){
    #create systemwide variable
    x <<- y
    m <<- NULL
  }
  
  # get the value of the matrix
  get <- function() x
  
  # set the value of the inverse of the matrix
  setinv <- function(solve) m <<- solve
  
  # get the value of the inverse of the matrix
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Return a matrix that is the inverse of x. Uses the cached data if available 

cacheSolve <- function(x, ...) {
 
  #check if the cached data is avaliable 
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # if not; creat it and set the inverse of x
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}