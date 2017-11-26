## These functions caches the inverse of a matix, as inversing a matrix
## is a costly computation and may benefit in having it in the cach.

## This function creates the spesial matrix object and the functions
## that is used to cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
                i <- NULL
                set <- function(y){
                        x <<- y
                        i <<- NULL
                }
                get <- function() x
                set.inverse <- function(inverse) i <<- Inverse
                get.inverse <- function() i
                
                list(set=set, get=get, set.inverse=set.inverse, get.inverse=get.inverse)

}


## This function computes the inverse of the matrix created in "makeCacheMatrix"
## function. If the inverse already has been computed, then this function
## retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$get.inverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$set.inverse(i)
        i
}
