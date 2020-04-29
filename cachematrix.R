## Caching the Inverse of a Matrix :
## Matrix inversion is usually a costly computation.
## So,caching the inverse of a matrix rather than compute it repeatedly is very useful.
## Here are functions which creates objects to store matrix and cache inverse

         ## 1. Function/Coding for creating matrix and caching its inverse

makeCacheMatrix <- function(x = matrix()) { 
     # x intializes function arguement, so no furthet intialization required within the function.
     # initially inv is set NULL, which can be used later
     # This line of code clears any value of inv that had been cached by a prior execution of cacheSolve()
	         inv <- NULL
     # set is modules that set (mutate) the data values within an object.
     # set takes arguemen y, which is assumed as matrix	
                 set <- function(y) {
     # operator <<- this assigns value in parent environment to the object left side of this operator
		 x <<- y
		 inv <<- NULL
	}
     #Since the symbol x is not defined within get(), R retrieves it from the parent environment of makeCacheMatrix	()
                 get <- function()x
	         setInverse <- function (inverse) inv <<- inverse
	         getInverse <- function() inv
     #Create a new object by returning a list()
	         list(set = set,
                      get = get,
                      setInverse = setInverse,
                      getInverse = getInverse)
}

          ##2. Function/Coding for inverse of the above matrix

## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.
## cacheSolve() is required to populate and/or retrieve the mean from an object of type makeCacheMatrix().

cacheSolve <- function(x, ...) {
     # Return a matrix that is the inverse of 'x'
     # retrieve a inverse from the object passed in as the argument.
	         inv <- x$getInverse()
     # if the value here is not equal to NULL, we have a valid, cached inverse and can return it to the parent environment
                 if(!is.null(inv)) {
                 message("getting cached data")
                 return(inv)
}
                 matr <- x$get()
     # if X is a square invertible matrix, then solve(X) returns its inverse.
                 inv <- solve(matr, ...)
                 x$setInverse(inv)
                 inv
}
