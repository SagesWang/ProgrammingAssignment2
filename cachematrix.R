## The function cacheSolve returns the inverse of the matrix created with the makeCacheMatrix function.
## If the cached inverse is available, cacheSolve retrieves it, and if not,
## it computes, caches, and returns it. thats the gist of this.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y){
                x <<- y
                inverse <- NULL
        }
        get <- function() x
        setinverse <- function(inver) inverse <- inver
        getinverse <- function() inverse
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)){
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data,...)
        x$setmean(inverse)
        inverse
}
