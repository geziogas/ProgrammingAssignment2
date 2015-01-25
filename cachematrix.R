# # In this assignment we have 2 functions.
# # The first function creates the cached matrix, i.e.
# # puts in the cache memory the matrix that we give as an 
# # argument in the beginning. Then by calling the "cacheSolve()"
# # function, we get the inversed matrix. If we run for 2nd time
# # the cacheSolve() function, we get a message, that we got the
# # inversed matrix from cache, instead of recalculating it.
makeCacheMatrix <- function(x = matrix()) {

    m <- NULL
    set <- function(y)
    {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    list(set = set, get = get,setInverse = setInverse,getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m)) 
    {
        message("getting cached inverse matrix")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}