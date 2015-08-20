## This function will return an inversed matrix of the input matrix, 
## if the input is unchanged, the function will look up in cached values to get 
## the inversed matrix instead of re-computing.

## Create and cache the special matrix to be solved

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function () x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse, getinverse= getinverse)
}


## Solve the cached matrix to get the inversed matrix

cacheSolve <- function(x, ...) {
        ## Look up in stored values to check if the inversed matrix is already stored or not
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        ## If the inversed matrix does not exist then compute the data to get it
        data <- x$get()
        i <- solve(data,...)
        ## Store the new inversed matrix into "setinverse"
        x$setinverse(i)
        ## Return a matrix that is the inverse of 'x'
        i
}

