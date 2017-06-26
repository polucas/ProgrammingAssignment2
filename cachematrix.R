## The following functions were designed with the aim of creating a matrix and  
## cache'ing it's mean. 

## makeCacheMatrix creates a special "matrix" object and contains a function to:
##    set - set the value of a matrix
##    get - get the value of a matrix
##    setInverse - set the inverse of a matrix
##    getInverse - get the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setInverse <- function(inverse) inv <<- inverse
      getInverse <- function() inv
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}

## cacheSolve computes the inverse of a special "matrix" created by the previous 
## funcion. If the inverse has been calcualted previously, then cacheSolve 
## function will retrieve the inverse from the cache - this operation can save
## a lot of time when working with very big and complex matrices. 

cacheSolve <- function(x, ...) {
      inv <- x$getInverse()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      m <- x$get()
      inv <- solve(m, ...)
      x$setInverse(inv)
      inv
}


m1<-matrix(c(2, 4, 4, 6),nrow=2,ncol=2)
m2<-matrix(c(1,2,1,3,1,4,1,5,1),nrow=3,ncol=3)
cm<-makeCacheMatrix(m1)
cm$get()
cacheSolve(cm)
cacheSolve(cm) ## second run - will use cached data ("getting cached data" appears)
cm$getInverse()
cm$set(m2)
cacheSolve(cm)
cacheSolve(cm)