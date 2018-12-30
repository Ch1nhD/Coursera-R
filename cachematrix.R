
#############################################################################
## Introduction
## Matrix inversion can be an time intensive computation if ran in a loop.
## Caching and pulling the result may be beneficial rather than performing 
## the repeated calculation.

#############################################################################
## 'makeCacheMatrix' function creates a special "matrix", which is
## a list containing functions to
##
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix
##
## The '<<-' operand assigns a value to an object in an environment separate
## from the current environment.


makeCacheMatrix <- function(x = matrix()){
		inversematrix <- NULL
		set <- function(y) {
				x <<- y
				inversematrix <<- NULL
		}
		get <- function() x
		setinverse <- function(inverse) inversematrix <<- inverse
		getinverse <- function() inversematrix
		list(set = set, get = get,
			setinverse = setinverse,
			getinverse = getinverse)
}

#############################################################################
## 'cacheSolve' function checks if the inverse has been previous calculated.
## If yes then the inverse matrix is pulled from cache.  
## else runs the 'solve' function and returns the value of inverse matrix


cacheSolve <- function(x, ...){		
		inversematrix <- x$getinverse()
		if(!is.null(inversematrix)) {
				message("getting cached data")
				return(inversematrix)
		}
		data <- x$get()
		inversematrix <- solve(data, ...)
		x$setinverse(inversematrix)

		## Return a matrix that is the inverse of 'x'
		inversematrix
}