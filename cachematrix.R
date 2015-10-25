## The below two functions are used to create a special matrix and store its inverse and 
## use the cache to retrive the matrix inverse which is a costly operation

## The function 'makeCacheMatrix' creates a special matrix that can cache its inverse
## It does the following operations 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  m_inv <- NULL
  set <- function(y){
    x <<- y
    m_inv <<- NULL
  }
  
  get <- function() x
  
  setInv <- function(inverse) m_inv <<- inverse
  getInv <- function() m_inv
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}

##The function 'cacheSolve' computes the inverse of the matrix creted in the above function.
##If the inverse is already calculated, it retrives the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x' if it is cached
  m_inv <- x$getInv()
  if (!is.null(m_inv)){
    message("getting cached data...")
    return(m_inv)
  }
  
  ## Return a matric that is the inverse of 'x' if it is not cached
  m_inv <- solve(x$get())
  x$setInv(m_inv)
  m_inv
}
