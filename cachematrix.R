## These functions are part of the Coursera programming assignment 2 and are the original
## work of David Harter.


## makeCacheMatrix creates a list containing functions to set and get the values of and to 
## set and get the inverse of the matrix we are working with.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) inv <<- solve
	getinverse <- function() inv
	list(set = set, get = get,
		 setinverse = setinverse,
		 getinverse = getinverse)
}


## cacheSolve calculates the inverse of the matrix, but first checks to see if this value
## has already been cached.  If so, it skips the calculation; if not, it performs the 
## calculation and saves the inverse matrix in the cache for later use.

cacheSolve <- function(x, ...) {
	inv <- x$getinverse()
	if(!is.null(inv)){
		message("Getting cached data...")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinverse(inv)
	inv
}
