## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a list to operate with a matrix and the cache

## cacheSolve makes use of the previous functions to retrieve the inverse
## matrix from the cache, or calculate ir and load it into cache if it
## was not previously loaded


## Write a short comment describing this function
## makeCacheMatrix initializes the cache (m) and loads a matrix (x)
## then creates a list of functions to work on the matrix and the cache

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        
        ## list of functions to use on matrix (x) and cache (m)
        
        ## loads a new matrix and clears the cache from previous inverse values
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ## gets the matrix to be inverted
        get <- function() x
        
        ## loads the inverse value into the cache (m)
        setinverse <- function(inverse) m <<- inverse
        
        ## gets the inverse value from the cache (m)
        getinverse <- function() m
        
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Write a short comment describing this function
## cacheSolve retrieves the inverse form cache (m) if already loaded
## otherwise calculates de inverse and stores it in cache (m) 

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        
        ## only for square matrix with determinant !=0
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
