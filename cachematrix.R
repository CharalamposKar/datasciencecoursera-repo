## this function creates a special matrix object 
## that stores the matrix and cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    
    inverse<-NULL
    
    set<-function(y){
                x<<-y
                inverse<<-NULL
    }
    
    get<-function() x
    
    setinverse<-function(inv) inverse<<-inv
    
    getinverse<-function() inverse
    
    list(set = set,get = get,setinverse = setinverse,getinverse = getinverse)
}

## this function calculates the inverse of a special matrix object
## or returns the inverse from the cache if it is already calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    Imatrix <- x$getinverse()
    
    if(!is.null(Imatrix)) {
      message("getting cached data")
      return(Imatrix)
    }
    
    data <- x$get()
    Imatrix <- solve(data, ...)
    x$setinverse(Imatrix)
    Imatrix
    
}
