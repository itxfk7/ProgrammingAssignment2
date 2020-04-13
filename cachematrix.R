## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
# sets the value for the matrix 
   
     x <<- y
    i <<- NULL
  }
  get <- function() x                              # simply gets value of the matrix
  setinverse <- function(inverse) i <<- inverse    # calculating inverse for the 1st time
  getinverse <- function() i                       # get the inverse when called
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")  # getting inverse from cache
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)        # Calculating inverse if not found in cache
  x$setinverse(i)
  i
}
