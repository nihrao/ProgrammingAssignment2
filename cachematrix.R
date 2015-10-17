## cachematrix.R (Used to calculate the inverse of a matrix. The inverse is cached as it
##                is computed. If the inverse of the same matrix is computed again, then
##                the cached matrix is loaded)

## This program has two functions : 1. makeCacheMatrix 2.cacheSolve



## 1. makeCacheMatrix : Used to 
##                            a.Set value to the matrix 
##                            b.Get value of the matrix
##                            c.Set value to the inverse
##                            d.Get value of the inverse

makeCacheMatrix <- function(x = matrix()) {
            xinv <- NULL
            set <-function(y) {
              x <<-y
              xinv <<- NULL
            }
            get <- function() x
            setinverse <- function(inverse) xinv <<- inverse
            getinverse <- function() xinv
            list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## 2. cacheSolve : Used to calculate the inverse of the matrix. This function also 
##                 retrieves the cached matrix from memory if the inverse was 
##                 previously computed

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  xinv <- x$getinverse()
  
  if (!is.null(xinv)) {
    print("Cached data loaded")
    xinv
  } 
  else {
    xinv <- solve(x$get())
    x$setinverse(xinv)
    xinv
  }
}