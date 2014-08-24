## CacheMatrix & CacheSolve is try to store computation intensive Matrix Inverse
## So that computed inverse matrices are just returned from cache if they are computed before.

## This funciton is try to cache a matrix and its inverse matrix after computation.
makeCacheMatrix <- function(x = matrix()) 
{
    inverse_m <- NULL
    get <- function() x
    set <- function(y)
    {
        x <<- y
        inverse_m <<- NULL
    }
    get_inverse <- function() inverse_m
    set_inverse <- function(inverse) inverse_m <<- inverse
    list(set=set, get=get, set_inversed=set_inverse, get_inversed=get_inverse)
}


## This function is intented to calculate inverse matrix given a CacheMatrix created by above function
## It is smartly return a cached inverse matrix if it exists, otherwise, computed via R solve function.
cacheSolve <- function(x, ...) 
{
    inverse <- x$get_inverse()
    if (!is.null(inverse))
    {
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$set_inverse(inverse)
    return(inverse)
}
