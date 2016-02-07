## These functions allow caching the inverse of a matrix in an environment different from the one where the matrix was specified.

## makeCacheMatrix creates a special matrix that caches its inverse.

makeCacheMatrix <- function(x = matrix())
{
    inverse <- NULL
    
    set <- function (y)
    {
        x <<- y
        inverse <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(invmatrix) inverse <<- invmatrix
    
    getinverse <- function() inverse
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve computes the inverse of the special matrix returned by makeCacheMatrix.

cacheSolve <- function(x,...)
{
    inverse <- x$getinverse()
    if(!is.null(inverse))
    {
        message("getting cached data")
        return(inverse)
    }
    
    data <- x$get()
    inverse <- solve(data)
    x$setinverse(inverse)
    inverse ## Return a matrix that is the inverse of 'x'
}