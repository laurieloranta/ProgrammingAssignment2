## This program consists of two functions: 
##
## 1. makeCacheMatrix creates a special "matrix" object that can cache its 
## inverse.This function returns a list of closure functions that are able 
## to (1) get and (2) set the matrix itself and (3) get and (4) set the inverse 
## of that matrix in the cache. 
##
## 2. The cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve retrieves 
## inverse from the cache.


## makeCacheMatrix takes matrix x as an optional argument. The function 
## assigns the value NULL to the local variable cacheInverse, which means that 
## the inverse matrix cache is initially empty. makeCacheMatrix returns a
## list which containts functions to set and get the matrix and 
## getInverse (read) and setInverse (write) the value of the local environment 
## variable cacheInverse.

makeCacheMatrix <- function(x = matrix()) {
    ## Initialize cache as NULL
    cacheInverse <- NULL
    ##Closure function for setting the matrix. CacheInverse is also set to NULL
    set <- function(y){
        x <<- y
        cacheInverse <<- NULL
    }
    #Closure function for getting the matrix
    get <- function(){
        return(x)
    }
    ## Closure function for setting the cacheInverse
    setInverse <- function(inverse) {
        cacheInverse <<- inverse
    }
    ## Closure function for getting the cacheInverse
    getInverse <- function() {
        cacheInverse
    }
    ## Return a list containing functions for set and get matrix and cachce
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The cahceSolve retrieves the inverse matrix from cachce. If the cache is 
## empty (NULL), the inverse is calculated and stored in to cahce. If the cache
## is not empty (!NULL) the function returns the inverse matrix from cache.

cacheSolve <- function(x, ...) {
    ## Read cache to local variable cacheInverse
    cacheInverse <- x$getInverse()
    ## If cache is empty (NULL) then calculate the inverse and store it in cache
    if (is.null(cacheInverse)) {
        cacheInverse <- solve(x$get())
        x$setInverse(cacheInverse)
    }
    ## Return the cache which now contains the inverse matrix in any case
    return(cacheInverse)
}
