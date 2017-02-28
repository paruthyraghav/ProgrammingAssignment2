## Put comments here that give an overall description of what your
## functions do

## function to create a "matrix" object able to cache it's own inverse
## the function contains a list of 
## 1. set and get the value of the matrix
## 2. set and get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(	set = set, 
        		get = get,
             	setinverse = setinverse,
             	getinverse = getinverse
             )
}


## cacheSolve is to compute the inverse of the "matrix" object
## returned by makeCacheMatrix. Provided the matrix hasn't changed and
## the inverse has been calculated before, the cacheSolve function
## will retreive the inverse from the cache

cacheSolve <- function(x, ...) 
{
        m <- x$getinverse()
        if(!is.null(m)) {
                			message("getting cached data")
                			return(m)
        				}
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
