## cachematrix is a script consisting of two functions which expedite 
## recomputing the inverse of a matrix. cacheSolve(makeCacheMatrix(x)) first
## checks whether the inverse of x has already been calculted; if so, it 
## returns that inverse. If not, it calculates, saves, and reports the 
## inverse of x.

## The first function, makeCacheMatrix, creates a special "matrix", 
## which is really a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the matrix inverse
## 4. get the value of the matrix inverse0

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## The function cacheSolve calculates the inverse of the special "matrix" created
## with the above function. However, it first checks to see if the inverse has already
## been calculated. If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in 
## the cache via the setsolve function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}