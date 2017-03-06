## Following two functions have been created

##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## Following is a short description of the function makeCacheMatrix
##Step 1-  It sets the value of a matrix that goes in (X is a square invertible matrix)
## Step 2 - It gets the value of the matrix
## Step 3 - sets the value of the inverse using the function Solve
## Step 4 - Gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The following function calculates the inverse of the matrix created with the above function ( makeCacheMatrix)
##However, it first checks to see if the inverse of a matrix has already been calculated. 
# If so, it gets the mean from the cache and skips the computation.
#Otherwise, it calculates the inverse of the matrix and sets the value in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
  ## Return a matrix that is the inverse of 'x'
}

