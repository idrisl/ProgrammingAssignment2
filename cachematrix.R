## These functions will return the inverse of a matrix and store 
## the result in the cache. 
## However if the result exists in the cache, the inverse will not be calculated.

## The first function will create a list containing a function to:
## 1. set the value of the vector
## 2. get the value of the vector
## 3. set the value of the mean
## 4. get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    
    setinv <- function(inverse)  inv <<- inverse
    getinv <- function() inv
    
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## This function will search and return the inverse of a given matrix from
## the cache (avoiding the calculation again). 
## If data does not exist, it will calculate the inverse of the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data...")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data,...)
    x$setinv(inv)
    inv
}