## Caching the inverse of a matrix:
## The function 'makeCacheMatrix()' creates a special matrix objects that 
## can cache its inverse and return it when asked for. Similarly, 
## the function 'cacheSolve()' first checks whether the matrix inverse has
## already been computed and cached or not. If yes, it retrieves the
## cached inverse and returns it else, it computes the inverse, caches it
## and then, return the inverse.

## The function 'makeCacheMatrix()' creates a special matrix objects that 
## can cache its inverse and return it when asked for. The function 'set()' 
## sets the matrix data in the special object. Similarly, 'setinverse()' 
## caches the inverse provided as argument. Functions 'get()' and 
## 'getInverse()' return matrix data and cached inverse of the matrix
## respectively.   

makeCacheMatrix <- function(x = matrix()) {
	matInverse <- NULL
	set <- function(y){
		x  <<- y
		matInverse <<- NULL
	}
	get <- function() x
	setInverse <- function(mat_inverse) matInverse <<- mat_inverse
	getInverse <- function() matInverse
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) 
}


## The function 'cacheSolve()' checks if the inverse of a matrix has
## already been solved and cached or not. If yes, it retrieves that cached
## inverse using function 'getInverse()' of the special matrix object.
## If not, it computes the inverse, using function 'solve()', caches the
## inverse using function 'setInverse()' of the special matrix object
## and providing it with recently computed inverse as argument, 
## and then return this inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matInverse <- x$getInverse()
        if(!is.null(matInverse)){
			message("getting cached data")
			return(matInverse)
		}
		data <- x$get()
		matInverse <- solve(data, ...)
		x$setInverse(matInverse)
		matInverse
}
