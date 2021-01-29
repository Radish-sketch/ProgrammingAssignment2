## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#########################################################################
# makeCacheMatrix creates a "vector", a list containing a function to
# 1. set the value of the vector
# 2. get the value of the vector
# 3. set the value of the inverse
# 4. get the value of the inverse
#########################################################################

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
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


## Write a short comment describing this function

#########################################################################
# cacheSolve calculates the inverse of the special "vector" created with 
# makeCacheMatrix. it first checks to see if the inverse has already been 
# calculated, if yes, it get the value then skips, otherwise calculates the
# inverse with solve() function. The matrix supplied is always invertible
#########################################################################


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
