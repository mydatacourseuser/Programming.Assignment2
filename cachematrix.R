## This function cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL 
        set <- function(y) { 
                x <<- y 
                m <<- NULL } 
        get <- function() x 
        setsolve <- function(solve) {m <<- solve} 
        getsolve <- function() m 
        list(set = set, get = get,setsolve = setsolve,getsolve = getsolve) 
                                        } 


        
        
        ## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse  

        cacheSolve <- function(x, ...) { 
                m <- x$getsolve() 
                if(!is.null(m)) {
                        message("getting cached data") 
                        return(m) 
                        } 
                
                data <- x$get() 
                m <- solve(data, ...) 
                x$setsolve(m) 
                m 
        }
        
 
      