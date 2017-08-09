## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: this function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        ## x is a square invertible matrix
        
        i <- NULL
        set <- function(y) {
                ## with <<- we assign a value to objects in an environment which is
                ## different from the current environment
                 x <<- y
                i <<- NULL
        }       ## set the matrix
        get <- function() x ## get the matrix
        setinverse <- function(inverse) i <<- inverse ## set the inverse
        getinverse <- function() i ## get the inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## cacheSolve: This function computes the inverse of the "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not 
# changed), then the cacheSolve retrieves the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x
        ## this functions takes the list of makeCacheMatrix as input
        i <- x$getinverse()
        ## if the inverse was already calculated then:
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        ## if not, calculate the inverse here:
        data <- x$get()
        i <- solve(x, ...)
        ## set the value of the inverse in the cache via the "setinverse" function
        x$setinverse(i)
        i
}


## The End

