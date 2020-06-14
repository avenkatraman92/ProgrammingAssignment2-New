## This function provides the option of caching the inverse value of a matrix in memory so that
## it need not be solved each time and can be called using the cacheSolve function.

## The makeCacheMatrix function provides us with a list of four sub-functions - get, set,
## setinverse and getinverse which are loaded to the parent environment when the function called.
## These functions initiate the placeholders for matrix and inv and then call inverse and store in
## SetInverse and GetInverse

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL 

set <- function(y)
  x <<- y
inv <<- NULL

get <- function() {x}
setinverse <- function(inverse) {inv <<- inverse}
getinverse <- function() {inv}

list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## This function caches the inverse value of a matrix by loading the $getinverse of matrix in inv 
## if inv has not been solved then this function goes on to calculate the inverse of matrix and 
## stores it in inv and returns inv. 

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if (!is.null(inv)) { 
    message("getting cached inverse")
    return(inv)}
  
  matr <- x$get()
  inv <- solve(matr)
  return(inv)
        ## Return a matrix that is the inverse of 'x'
}

