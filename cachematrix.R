## This is the function that can calculate and cache the inverse of a matrix x. 
## The following function is useful to speed up the time-comsuming repeating computations of a large data set.

## Usage: 	testmatrix <-makeCacheMatrix(inputmatrix)
##			cacheSolve(testmatrix)  ## first time it calculate the inverse matrix
##			cacheSolve(testmatrix) 	## second time it returns the cached matrix

## The makeCacheMatrix function creates a special matrix containing a function to 
## 			1. set the value of the matrix
##			2. get the value of the matrix
##			3. set the value of the inverse
##			4. get the value of the inverse		

makeCacheMatrix <- function(x = matrix()) {
	invm <- NULL
	set <- function(y) {
		x <<- y
		invm <<- NULL
	}
	get <- function() x ## return the input matrix x
	setinverse <- function(inverse) invm <<- inverse
	getinverse <- function() invm
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The cacheSolve function first check if the inverse value has already been calulated, 
## if so, it returns the inverse matrix from the cache and skip the computation.
## If not, it calculates the inverse of the matrix, and sets the matrix in the cache.

cacheSolve <- function(x, ...) {
        ## 
        invm <- x$getinverse()
        ## If invm exist, return the cached inverse matrix 
        if (!is.null(invm)){
        	message("getting cached data")
        	return(invm)
        }
        ## Otherwise calculate the inverse matrix
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m) ## save the value in the cache by setinverse function
        m
}
