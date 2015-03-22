## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## initiates the m variable in the cahe and
## creates a list of functions to work on the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Write a short comment describing this function
## if getinverse is not null (m in cahe alredy has a value) gets that value
## otherwise calculates de inverse and stores it in cache (m) 

cacheSolve <- function(x, ...) {
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
