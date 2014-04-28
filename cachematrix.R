## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function:
## Here we define the function to create an object that contains 
## information about a matrix and also its inverse. This object
## contains 4 functions: "set" that it's used when creating the 
## object; "get" that allows you to retrieve the simple matrix; 
## "setinverse" that can store the value of its inverse; and
## "getinverse" that allows you to retrieve the inverse if it
## has been calculated (if not it is set to NULL);
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function:
## cacheSolve calculates the inverse of an object of those
## created by makeCacheMatrix. If the inverse of the object
## has never been calculated yet, it applies solve to the matrix
## and store the inverse in the object (via "setinverse"). If the 
## inverse has been obtained before, it retrieves it from the
## stored value in the object (via "getinverse").

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if (!is.null(i)){
    message("getting cached data!")
    return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}