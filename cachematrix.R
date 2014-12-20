## Functions for programming assignment 2


## This function creates a set/get the value of a matrix
## And set/get the values of the matrix's inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
	 x <<- y
	 m <<- NULL
    }
    get <- function() x
    setmtx <- function(solve) m <<- solve
    getmtx <- function() m

    list(set = set, get = get,
	 setmtx = setmtx,
	 getmtx = getmtx)
}


## Computes the inverse of the mAtrix created in the previous function
## If the inverse has already been calculated then gets the data from cache
## and retrn it.

cacheSolve <- function(x=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getmtx()
	if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmtx(m)
        m
}
