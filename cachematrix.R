## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    ## creating an initial variable for the inverse matrix with NULL
    i <- NULL
    ## Setting a matrix
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    ## getting the matrix value and assigning it to get
    get <- function() x
    ## setting the inverse matrix
    setinverse <- function(inverse) i <<- inverse
    ## getting the inverse matrix
    getinverse <- function() i
    ## The output
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## setting i equal to getinverse of x
    i <- x$getinverse()
    ## checking whether i is null or it has a value
    if(!is.null(i)) {
        ## pringint the message if the inverse already exists
        message("getting cached data")
        return(i)
    }
    ## otherwise calculating the inverse matrix
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
