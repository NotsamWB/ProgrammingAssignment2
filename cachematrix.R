## Computes the inverse of an invertible matrix
## if matrix already computed retrieves result from
## cache varaible m instead of recalculating it

## create a list that will contain the cached matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    set <- function(y){
            x <<- y
            m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## If variable M has already been computed returns the cache variable
## else call Solve function to calculate matrix inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data,...)
    x$setinverse(m)
    m
}
