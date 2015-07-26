## Put comments here that give an overall description of what your
## functions do

## This function creates a special matrix chan can be
## used to create its inverse

makeCacheMatrix <- function(x = numeric()) {
        cache <- NULL
        setMatrix <- function(newValue){
                x <<- newValue
                cache <<- NULL
        }
        
        getMatrix <- function() x
        cacheInverse <- function(solve) cache <<- solve
        getInverse <- function() cache
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             cacheInverse = cacheInverse,
             getInverse = getInverse)
}

## This function computes the inverse of the special matrix
## If the inverse is already created, then the function retrieves
## the inverse from the cache

cacheSolve <- function(y, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inverse <- y$getInverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- y$getMatrix()
        inverse <- solve(data)
        y$cacheInverse(inverse)
        inverse
}
