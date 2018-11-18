## Caching the Inverse of a Matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	iv <- NULL
	set <- function(y) {
		x <<- y
		iv <<- NULL
	}
	get <- function() x
	setiv <- function(inverse) iv <<- inverse
	getiv <- function() iv
	list(set=set, get=get, setiv=setiv, getiv=getiv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then this function retrieves the inverse from the cache.

cacheSolve <- function(x) {
	iv <- x$getiv()
	if(!is.null(iv)) {
		message("getting cached data")
		return(iv)
	}
	data <- x$get()
	iv <- solve(data)
	x$setiv(iv)
	iv
}
