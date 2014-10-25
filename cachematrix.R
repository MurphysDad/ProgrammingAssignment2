## The first function, makeCacheMatrix first sets imat (inverted matrix) to
## NULL in the environment of the function, i.e. to be a local variable.
## four functions are then defined and returned from this function in a list
## for use in manipulating the input matrix and the inverted result 

## The function 'makeCacheMatrix' creates matrix as defined by user in function argument
## These functions then are used to (re)set, or get the resulting matrix or inverted result (imat)
## If set, the external stored result is nulled (cleared) so it will not be retrieved.
## Setimat is called from the second cacheSolve function to cache the result

makeCacheMatrix <- function(x = matrix()) {
	imat <-  NULL

	set <- function(y) {
		x    <<- y
		imat <<- NULL                 ## reset matrix so nullify externally cached inversion
}
	get    <- function() {
		     x                        ## return the made matrix
}
	setimat <- function(matrix) {
		imat <<- matrix               ## caches inverted matrix
}
	getimat <- function() {             ## return inverted matrix   
		imat
}
	list(set = set, get = get, setimat = setimat , getimat = getimat) 
}

## This function inverts the matrix returned from the first function above but only if is null
## in the cache. If a result is 'found' then this can be reused as it would have been nulled if a
## new matrix had been created

cacheSolve <- function(x, ...) {

	 matrix <- x$get()
	 imat   <- x$getimat()
	 if (!is.null(imat)) {   ## if locally stored imat is null will return externally stored value 
		message("retrieving cached inverted matrix")
		return(imat)       ## exit function and return , i.e. don't invert again ? 
	 }
	 message("about to invert the matrix")
	 data <- x$get()
	 imat <- solve(matrix)
	 x$setimat(imat)
	 imat
}



