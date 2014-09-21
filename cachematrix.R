## Cache inverse matrix of a matrix.
## makeCacheMatrix :  Creates the object
## cacheSolve : Calculate or retrieve inverse matrix


## Create a special matrix object that caches its inverse
makeCacheMatrix <- function(x = matrix()) {
        ## Cache
        inversa <- NULL
        
	## Assign a new object
        set <- function(y) {
                x <<- y
                
                ## Reset cache
                inversa <<- NULL
        }
        
        get <- function() x
        
        ## Set cache
        setInv <- function(i) inversa <<- i
        
        ## Get cache
        getInv <- function() inversa 

        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## Get the cache inverse or calculate the inverse of matrix, stored in the special object pass as parameter
cacheSolve <- function(x, ...) {
	
	## Get the inverse if it was calculated before
        i <- x$getInv()
        
        ## If inverse doesn't exists... (not calculate yet)
        if(is.null(i)) {
   		## Get matrix
   		data <- x$get()

        	## Calculate inverse matrix 
        	i <- solve(data)

		## Store inverse (cache)
        	x$setInv(i)
        }
	
	## Return inverse matrix
        i
}