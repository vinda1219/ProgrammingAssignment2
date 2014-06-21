## functions makeCacheMatrix and cacheSolve work together to
## solve inverse of matrix and cache the result

## make cache of a matrix
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	
	## set a new matrix y
	set <- function(y){
		x <<- y
		inv <<- NULL    
	}
	
	## get matrix data
	get <- function() x
  
    ## cache inverse of matrix 'x'
	setinverse <- function(inverse) inv <<- inverse
  
    ## get inverse of matrix 'x'
	getinverse <- function() inv
  
    ## create a list which contains infors of matrix 'x'
	list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Solve matrix if it hasn't been solved;otherwise, return 
## its inverse
cacheSolve <- function(x, ...) { 
	inv <- x$getinverse()
    
	## matrix has been solved
	if(!is.null(inv)){
		message("getting cached data")
		return(inv) 
	}
  
    ## matrix hasn't been solved, get matrix first
	matrix <- x$get()
	## solve matrix
	inv <- solve(matrix,...)
	## cache inverse of matrix
	x$setinverse(inv)
	
    ## Return a matrix that is the inverse of 'x'
	inv
}


