## Put comments here that give an overall description of what your
## functions do

y <- matrix(c(1,3,6,0,5,1,8,6,5),3,3)
y1<-makeCacheMatrix(y)
cacheSolve(y1)



## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    print(solve(m))
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  print(m)
  solve(m)
}


