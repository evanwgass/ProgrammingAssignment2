## The first function creates a matrix object, and sets/gets 
## (changes and stores) the value of the "inverse" object to be applied later.
## Subsequently, the second function recalls the objects from the first, 
## and calcluates then returns the inverse of the matrix "x".

## makeCacheMatrix creates and stores the set, get, setinv, and getinv functions.
## "Y" allows for a substitution to the "x" (matrix input) argument, while "i" is initialized to NULL.
## This allows "i" to be dynamic in case the input value changes.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) i <<- inverse
  getinv <- function () i 
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function takes as an argument an object where makeCacheMatrix is stored.
## It then checks the value of "i" (recalled from the prior function). If it is a NULL value,
## the function then recalls the value of the "x" matrix, and calculates the inverse, stored as "i".
## If "i" was not a null value, however, it will return the previous value of "i".

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinv(i)
  i       
}
