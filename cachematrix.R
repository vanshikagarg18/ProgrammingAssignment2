## There are two functions makeCacheMatrix, makeCacheMatrix
## makeCacheMatrix consists of set, get, setinv, getinv
##library(MASS) is used to calculate the inverse for all non square as well as square matrices
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  ##intiliazing inverse as NULL
  inv <- NULL 
  set <- function(y){
    x<<-y
    inv<<- NULL
    }
  ## function to get matrix x
  get <- function()x
  setinv <- function(inverse)inv <<- inverse
  getinv <- function(){
    inver <- ginv(x)
    ##to obtain inverse of the matrix
    inver%*%x
    }
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

##This is used to get the cache data
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  ##checking whether the inverse is NULL or not
  if(!is.null(inv)){
    message("getting cached data")
    ##returns inverse value
    return(inv)
    }
  data<-x$get()
  ##calculates inverse value
  inv<-solve(data,...)
  x$setinv(inv)
  ## Return a matrix that is the inverse of 'x'
  inv
}
