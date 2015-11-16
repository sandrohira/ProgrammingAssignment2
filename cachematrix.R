## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
## --------------------------------------------------------------------------------------
## [Usage example]:
### source("cachematrix.R")
### c <- rbind(c(2, 3), c(2, 5)) 
### tmp <- makeCacheMatrix(c)
### cacheSolve(tmp)


##makeCacheMatrix creates a special "matrix", containing following list of functions:
###	set: set the value of the matrix
###	get: get the value of the matrix
###	setinverse: set the value of the matrix inverse
###	getinverse: get the value of the matrix inverse

makeCacheMatrix <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(matrix) m <<- matrix
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}



## cacheSolve function calculates the inverse of the special "matrix" created with the makeCacheMatrix function. 
## Prior to calculating the inverse itself, it checks if inverse has been calculated previously. If so, it gets inverse from the cache and skips the calculation
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
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