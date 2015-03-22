#|----------------------------------------------------------------------------------
#| Programming Assignment 2: Caching the Inverse of a Matrix
#| 
#| https://github.com/davidchuey/ProgrammingAssignment2
#| Description: Function which caches the inverse result set of a matrix.
#|----------------------------------------------------------------------------------


#|----------------------------------------------------------------------------------
#| makeCacheMatrix - creates a special "matrix" that is able to cache its 
#| inverse.  Returns a list containing functions to: 
#| 1.) set the value of the matrix
#| 2.) get the value of the matrix
#| 3.) set the value of the inverse of the matrix
#| 4.) get the value of the inverse of the matrix
#|----------------------------------------------------------------------------------

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse 
    getinverse <- function() inv
    list(
        set = set
        ,get = get
        ,setinverse = setinverse
        ,getinverse = getinverse
    )
}


#|----------------------------------------------------------------------------------
#| cacheSolve - function which calculates the inverse of the special "matrix" 
#| that is returned from makeCacheMatrix function.  cacheSolve first checks if 
#| the inverse has been calculated which is retrieved by the cache and as a result
#| skips the actual computation.  If not, then cacheSolve calculates the inverse of 
#| the matrix and sets it in the cache. 
#|----------------------------------------------------------------------------------

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv = x$getinverse()
    if (!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    return(inv)
}
