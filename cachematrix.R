# makeCacheMatrix: This function creates a special "matrix" object that
# can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
# x is a variable of the makeCacheMatrix function. 
# x is initally set as an empty matrix. 
        
        i <- NULL
        
        # i is a variable initially set to NULL.  
        # Initially. the NULL indicates that there is no stored inverse
        # of the matrix (IOTM).
        # i will eventually hold the cached IOTM. 
                
        set <- function(y) {    
 
        # Create a function set() with variable y which is required before 
        # the IOTM can be calculated.
                
                x <<- y         
                
                # Assigns y to the x matrix in the global environment.
                
                i <<- NULL      
                
                # Since we may now have a new i matrix, reset i to NULL 
                # since the value (inverse) may need to be recalulated.
                
        }
        
        get <- function() x     
        
        # Return the value of x stored in the makeCacheMatrix object.
        
        setinverse <- function(inverse) i <<- inverse 
        
        # Set the value of i as a parameter called inverse.
        
        getinverse <- function() i

        # Return the value of i stored in the makeCacheMatrix object.
 
        # Four functions are now defined to access x and i within the 
        # makeCacheMatrix object.
        
        list(set = set,       
             get = get,       
             setinverse = setinverse,
             getinverse = getinverse)
        
        # Take the list of functions and tag them with the same name as 
        # the function itself.        
}

# cacheSolve: This function computes the inverse of the special "matrix"
# returned by makeCacheMatrix.

cacheSolve <- function(x, ...) {
        
        i <- x$getinverse()     
        
        # Use the getinverse() function in the makeCacheMatrix object to 
        # store the cached IOTM in the local variable "i."
        
        if(!is.null(i)) {       
                
        # If i is not NULL, then there is a cached IOTM.
                
                message("getting cached data")
                
                # Print the message "getting cached data."
                
                return(i)       
                
                # Return the IOTM. Done.
                
        }
                
        data <- x$get()
        
        # If i is NULL, there is no cached IOTM, so, using the get() 
        # function in the makeCacheMatrix object, assign the x matrix 
        # to the local variable "data."
                
        i <- solve(data, ...)   
        
        # Calculate the inverse of the matrix, 
        
        x$setinverse(i)         
        
        # Cache the newly caclulated IOTM in makeCacheMatrix.
        
        i                       
        
        # Return the newly calculated IOTM.
        
}
