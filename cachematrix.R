## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly. The
## following two functions are used to cache the inverse of a matrix.

 
## makeCacheMatrix creats a list, which is a list containing a function to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  
  get <- function()x
  setinverse <- function(solve)m <<- solve
  getinverse <- function()m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}



## the following function calculates the inverse of the special matrix created with the above function.  
## However, it first checks to see if the inverse has already been calculated.  if so, it gets the inverse
## from the cache and skips the computation.  Otherwise, it calculates the inverse of the data and sets the
## value of the inverse in the cache via the setinverse function

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return (m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

git push master
