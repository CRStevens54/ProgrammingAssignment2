## Put comments here that give an overall description of what your
## functions do

## This function makes a special matrix that is invertible.  It returns a list containing functions that will 1. set the matrix, 2. get the matrix, 3. set the inverse, 4. get the inverse


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x<<-y
                inv <<- NULL
        }
        get <- function() x;
        setinv <- function(inverse) inv<<-inverse;
        getinv <- function() inv;
        return(list(set = set, get = get, setinv = setinv,getinv = getinv))

}


## This function takes the ouput of makeCacheMatrix and returns the inverse of the matrix to makeCacheMatrix()
##If inverse was already calculated the computation is skipped and taken from the cache

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("Getting cached data...")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        return(inv)
}
