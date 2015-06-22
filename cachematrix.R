# Ben Bearce

## cacheSolve and makeCacheMatrix work together to take a matrix
## and create an inverse and store the result

## makeCacheMatrix takes a matrix and defines the necessary functions
## needed to cache a matrix's inverse 

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) {i <<- inverse}
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## takes makeCacheMatrix and input matrix argument
## and caches the matrix inverse after solving for it

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
