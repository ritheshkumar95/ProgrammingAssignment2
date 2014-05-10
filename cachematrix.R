## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#makeCacheMatrix creates functions to set,get the "special matrix" and to set, get the inverse matrix
makeCacheMatrix <- function(x = matrix()) 
{
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(invmat) inv <<- invmat
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

#cacheSolve function is used to solve the matrix to get the inverse when the inverse has already not been set
cacheSolve <- function(x, ...) 
{
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}
