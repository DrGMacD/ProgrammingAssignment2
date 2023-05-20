## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  set <- function(y) {
    x <<- y #annotations for inverse
  get <- function() x
  setInverse <- function() inv <<- solve(x) #calculate the inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)} 
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" created by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting inversed matrix")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv}
}
##Testing function:
## Firstly, set appropriate working directory.
setwd("~/GitHub/ProgrammingAssignment2")

test_matrix<-makeCacheMatrix(matrix(1:4,2))

test_matrix$get()
[,1] [,2]
[1,]    1    3
[2,]    2    4

test_matrix$setInverse()
test_matrix$getInverse()
[,1] [,2]
[1,]    1    3
[2,]    2    4

> my_matrix$getinv()
NULL

cacheSolve(test_matrix)
[,1] [,2]
[1,]   -1  -3
[2,]   -2  -4