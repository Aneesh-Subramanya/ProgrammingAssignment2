## Library with functions supporting caching of a matrix and its inverse

## Special matrix creation function
## This function allows caching of matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(matrix) {
            x <<- matrix
            inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inverse <<- solve
        getinverse <- function() inverse
        list(
                set = set, 
                get = get, 
                setinverse = setinverse, 
                getinverse = getinverse
        )
}


## Special matrix inverse calculation function
## This function checks if the matrix inverse has been computed
## already. If so, it retrieves the cached data, thus avoiding recomputation.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        matrix <- x$get()
        inverse <- solve(matrix, ...)
        x$setinverse(inverse)
        inverse
}