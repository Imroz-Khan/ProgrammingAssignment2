##These functions take advantage of scoping rules to avoid computation of inverse of a matrix
## by caching the inverse of the input matrix.

##This function takes the matrix as argument and returns a list of four functions
## set function stores the matrix value
## get function is used to retrieve the matrix
## setinverse stores the inverse of the matrix
## getinverse retrieves the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) m <<- inverse
	getinverse <- function() m
	list(set = set, get = get,
	      setinverse = setinverse,
	      getinverse = getinverse)
}


## This function takes the input matrix and then checks whether the inverse
## of the matrix is already computed or not
## If it is already computed then it returns a message "getting cached data"
## and then returns the already computed inverse
## else it computes the inverse using solve command and then set the inverse
## value to the list component setinverse.

cacheSolve <- function(x, ...) {
	m <- x$getinverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
        ## Return a matrix that is the inverse of 'x'
}
