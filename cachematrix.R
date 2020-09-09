## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix<-function(x = matrix()) {
  
  invval <- NULL
  set <- function(y) {
    x <<- y
    invval <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) invval <<- inverse
  getInverse <- function() invval
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
##This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invmat <- x$getInverse()
  if (!is.null(invmat)) {
    message("getting cached data")
    return(invmat)
  }
  mat <- x$get()
  invmat <- solve(mat, ...)
  x$setInverse(invmat)
  invmat
}
matrix1<-makeCacheMatrix(matrix(c(2,4,5,6), 2, 2))
matrix1$get()
matrix1$getInverse()
cacheSolve(matrix1)
cacheSolve(matrix1)
matrix1$getInverse()
matrix1$set(matrix(c(4,6,8,9),2,2))
matrix1$get()
matrix1$getInverse()
cacheSolve(matrix1)
cacheSolve(matrix1)

