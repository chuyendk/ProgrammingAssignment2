## makeCacheMatrix function creates a special "matrix" object 
## that can cache its inverse.

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(invX) inv <<- invX
  getInv <- function() inv
  list(set=set, get=get, setInv = setInv, getInv = getInv)  
}


## cacheSolve function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'`
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached matrix")
    return(inv)
  }
  data <- x$get()
  ##assume that the matrix supplied is always invertible.
  inv <- solve(data)
  x$setInv(inv)
  inv
}

## Test case
## m <- matrix(c(3,1,2,1), nrow=2, ncol=2)
## mat = makeCacheMatrix(m)
## mat$get()
## cacheSolve(mat) ## first time
## cacheSolve(mat) ## second time
## m %*% cacheSolve(mat) 
## m %*% cacheSolve(mat)
