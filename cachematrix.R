## Put comments here that give an overall description of what your
## functions do
## These function can cache a matrix inverse to avoid repeating an inverse.

## Write a short comment describing this function
## This function returns a list of functions to 
##	1. set a matrix
##	2. get a matrix
##	3. set the inverse matrix
##	4. get the inverse of the matrix
## Calling this with a square matrix will cache it in the returned 'cached matrix'
makeCacheMatrix <- function(x = matrix()) 
{
	inverse <- NULL
	## The set-function defined
	set <- function(y)
	{
		## Only set if this is a square matrix
		if( nrow(y)==ncol(y) )
		{
			x       <<- y
			inverse <<- NULL
		} else
		{
			print("Warning: Cannot set a non-square matrix")
		}
	}
	
	## The get-function defined
	get <- function() x

	## The setinverse-function defined
	setinverse <- function(inv) inverse <<- inv

	## The getinverse-function defined
	getinverse <- function() inverse
	
	## Create the returned list of functions
	list( set = set
		, get = get
		, setinverse = setinverse
		, getinverse = getinverse)
}


## Write a short comment describing this function
## This function calculates the inverse of a 'cached matrix'
cacheSolve <- function(x, ...) 
{
	inv <- x$getinverse()
	if(!is.null(inv))
	{
		message("getting cached inverse")
		return (inv)
	}
	mat <- x$get()
	inv <- solve(mat)
	x$setinverse(inv)
    ## Return a matrix that is the inverse of 'x'
	inv
}
