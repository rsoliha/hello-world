
## Program first checks the cache for inverse, if available, returns inverse directly..
## If not, calculates and returns the inverse of the matrix

## The first function defines 4 functions get, set, getinv, setinv. The get func
## returns the matrix, set creates a special object for the matrix, getinv returns
## inverse if available in cache and setinv adds the inverse to the cache


makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y)
	{
		x <<- y
		m <<- NULL
	}
	get <-function() x
	setinv <- function (solve) m <<-solve
	getinv <- function () m
	list (set=set, get=get, setinv=setinv, getinv=getinv)
}


## Gets the inverse of matrix. If null, computes inverse. If inverse
## already exists in cache, returns inverse.

cacheSolve <- function(x, ...) {
	m <- x$getinv()
	if(!is.null(m))
	{
		message("GETTING FROM CACHE")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinv(m)
	m

}
