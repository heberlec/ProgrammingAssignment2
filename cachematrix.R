##This file contains two functions that together allow for matrix
##inverse computation with caching. The first, makeCacheMatrix, 
##takes a normal matrix as its input and returns a CacheMatrix
##capable of caching its inverse. The second, cacheSolve,
##operates on CacheMatrices and returns the matrix inverse,
##using the cached value if available.

##makeCacheMatrix -- Returns a CacheMatrix object (really a list
##of functions) capable of caching its own inverse for 
##efficiency purposes
makeCacheMatrix <- function(mat = matrix()) {
	#Internally, our CacheMatrix object holds two fields
	#   mat - the matrix data
	#   inv - the inverse of mat if it's been computed; NULL otherwise
	#
	#In addition, it exposes four functions allowing
	#the user to view and manipulate those fields
	#   set - to update the matrix data ('mat')
	#   get - to retreive the matrix data ('mat')
	#   setinverse - to set the matrix inverse ('inv')
	#   getinverse - to retreive the matrix inverse (if available)

    #Initialize inv
    #Note that we don't have to initialize mat here, as it's passed in
    #as an argument
    inv <- NULL
    
    #Function definitions
    set <- function(y){
        mat <<- y
        inv <<- NULL
        #Note that we must reset inv to NULL when updating mat,
        #as changing the underlying matrix data invalidates
        #any previously cached computation of the inverse
    }
    get <- function() mat
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    
    #Construct the CacheMatrix object as a list of its component functions
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Computes the inverse of a CacheMatrix object, using the cached
## value of the inverse if available
cacheSolve <- function(cacheMat){
    #First attempt to retrieve the inverse from the cache
    inv <- cacheMat$getinverse()
    
    #If this succeeds, we return early
    if(!is.null(inv)){
        message("retrieving cached inverse")
        return(inv)
    }
    
    #Otherwise, retrieve the matrix data from the special 
    #cacheMatrix object, and compute the inverse
    mat <- cacheMat$get()
    inv <- solve(mat)
    
    #Make sure to update the cache for future computations
    #before returning the value
    cacheMat$setinverse(inv)
    inv
}
