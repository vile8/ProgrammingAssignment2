## Matrix inversion functions that provide caching to avoid repeated computation which is an expensive operation

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL

	##set external reference
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}

	##create function to get matrix data that was passed in
	get <- function() {
		#print("fetching data since there is no cache")
		x
	}

	##function to push the solved matrix into the cache (tho technically would cache whatever you stuff in "solvedmatrix"
	setinv <- function(solvedmatrix) {
		#print("Solving matrix and storing to cache")
		inv <<- solvedmatrix
	}

	##function to get cached inversion
	getinv <- function() inv

	##return methods for object
	list(set = set, 
	     get = get,
	     setinv = setinv,
	     getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {

	##use "ret" as the "return" variable, will also be used to store computed value if not in cache
	ret <- x$getinv()
	if(!is.null(ret)) {
		##For VALIDATION only, cached result exists, simply return
		#print("returning cached val")

        	## Break out of processing and Return a matrix that is the inverse of 'x'
		return(ret)
	}

	## No cache so get the matrix and store in var "data"
	data <- x$get()

	##invert the matrix
	ret <- solve(data)

	##store the cached results
	x$setinv(ret)	

	##return the value
	ret
}

testSolution <- function() {
	#Using the matrix based on inverse formula from: https://www.mathsisfun.com/algebra/matrix-inverse.html
	#just to ensure solve is working as expected
	matrixVals = c(4,2,7,6)

	#create the initial matrix and print its contents
	testMatrixCache <- matrix(matrixVals, nrow=2, ncol=2)
	print(testMatrixCache)
	
	#push the object into the cache
	cacheTheMatrix <- makeCacheMatrix(testMatrixCache)

	#invert the matrix without a pre-existing cache
	print("Run 1: ")
	computeInverse1 <- cacheSolve(cacheTheMatrix)
	print(computeInverse1)

	#run again, but this time we pick up the already inverted matrix from the cache
	print("Run 2: ")
	computeInverse2 <- cacheSolve(cacheTheMatrix)
	print(computeInverse2)

}


