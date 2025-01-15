## makeCacheMatrix creates a matrix, which is really a list containing a
## function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  matinv <- NULL
  set <- function(y) {
    x <<- y
    matinv <<- NULL
  }
  get <- function() x
  setinv <- function(minv) matinv <<- minv
  getinv <- function() matinv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve calculates the inverse of the special "vector" created with the
## above function. However, it first checks to see if the inverse has already
## been calculated. If so, it gets the inverse from the cache and skips the
## computation. Otherwise, it calculates the inverse of the data and sets the
## value of the inverse in the cache via the setinv function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
