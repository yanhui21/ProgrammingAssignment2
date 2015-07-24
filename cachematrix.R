## The funtion makeVacheMatrix creates a special matrix, which is a list
## containing  funtions  
## 1 funtion setMatrix sets the value of the matrix
## 2 funtion getMateix gets the value of the matrix
## 3 funtion setInverse sets the value of the inverse
## 4 funtion getInverse gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  I<-NULL
  setMatrix <- function(y) {
       y<<-x
    I<-NULL
  }
  getMatrix <- function() x
  setInverse <- function(MI) I<<-MI
  getInverse <- function() I
  
  list(setMatrix=setMatrix, getMatrix=getMatrix,
       setInverse=setInverse,getInverse=getInverse)
}


##function cachesolve compute the invers of the matrix created with the funtion
##makeCacheMatrix.it first checks to see if the inverse has been calculated. if so,
##it gets the inverse from the cache and skip the calculation. Otherwise, it
##calculates the inverse of the data and set the value of the inverse in the cache
## via setInverse function.

cacheSolve <- function(x, ...) {
  I<-x$getInverse()
  if(!is.null(I)) {
    message("get cached value")
    return(I)          
          
  }
  data<-x$getMatrix()
  I<-solve(data,...)
  x$setInverse(I)
  return(I)
}


