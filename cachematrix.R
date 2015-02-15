## A pair of functions that demonstrate caching of an inverse version of a
## passed matrix


##This function is used to create a cacheable version 
##of the inverse of the passed matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function() x
    
    setinverse <- function(x) i <<- x
    getinverse <- function() i
    
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


##This function returns the inverse of the passed matrix, if it has already
##been solved, it returns the results from the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    i <- x$getinverse()
    
    if (!is.null(i)){
        message("getting cached inverse matrix")
        return (i)
    }
    
    #get source matrix
    data <- x$get ()
    
    #get inverse of source matrix
    i <- solve(data)
    
    #cache inverse matrix
    x$set(i)
    
    #return inverse matrix
    i
}
