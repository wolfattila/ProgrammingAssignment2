## Here is the 3 command what I will have to use to test
## first create a matrix, then cache the origin Matrix
## and finaly run cache twice

y <- matrix(c(1,3,6,0,5,1,8,6,5),3,3)
y1<-makeCacheMatrix(y)
cacheSolve(y1)
cacheSolve(y1)



## So here we cache the origin Matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
## create the cache of MAtrix with empty inverse
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## for 1st tim eit save the invers, 
## 2nd time print from cahce

cacheSolve <- function(x, ...) {
  m <- x$getinv()
## check if the inverse was cached before
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
## cache the inverse
  x$setinv(m)
  print(m)
}


