## The following functions are used to create a special object 
## that stores a matrix and caches its inverse.

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
# It creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) 
{
    inv <- NULL
    
    # setting the value of the matrix
    set <- function(y) 
    {
        x <<- y
        inv <<- NULL
    }
    
    #getting the value of the matrix
    get <- function() 
        x
        
    #setting the value of inverse of the matrix
    setinverse <- function(inverse) 
        inv <<- inverse
        
    #getting the value of inverse of the matrix    
    getinverse <- function() 
        inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## NOTE : function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) 
{
    ## Returning a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) 
    {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
