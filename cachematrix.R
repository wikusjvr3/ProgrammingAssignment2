## Put comments here that give an overall description of what your
## functions do
# To preserve the state of R it is useful to manipulate the scoping rules of R.
# Essentially, being able to cache a value to an object outside of the immediate 
# environment allows that value to be saved for later use, which leaves R to 
# only perform new calculations that has not been cached -thereby saving memory.

## Write a short comment describing this function
# The first function creates a special "matrix" object that allow for its
# inverse to be cached. Basically, creating a list of cache functions! 
makeCacheMatrix <- function(x = matrix()) {
    # 1. set the value of the matrix    
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    # 2. get the value of the matrix
    get <- function(){
        x
    } 
    
    # 3. set the value of the inverse
    setinverse <- function(inversion){
        i <<- inversion   
    }
    
    # 4. get the value of the inverse    
    getinverse <- function(){
        i  
    } 
    
    # 5. creates a list of the above functions 1-4  
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
# This function calculates the the inverse of the special "matrix" returned by 
# makeCacheMatrix. If the inverse has already been calculated (and the matrix
# has not changed), then the cachesolve retrieves the inverse from the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         #  This function assumes the matrix supplied is always invertible.
    
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}

# Example
# Create Matrix that must be squared to obtain the inverse
# > x = matrix(sample(1:9),3,3)
# 
# Apply the cache function to the object 
# > m = makeCacheMatrix(x)

# View the object before inversion
# > m$get()
# [,1] [,2] [,3]
# [1,]    4    5    3
# [2,]    1    2    8
# [3,]    7    9    6

# Cache the Inverse of the Object
# > cacheSolve(m)
# [,1] [,2] [,3]
# [1,]   12  0.6 -6.8
# [2,]  -10 -0.6  5.8
# [3,]    1  0.2 -0.6

# If already cached, message will appear and provide the cached answer instead 
# > cacheSolve(m)
# getting cached data
# [,1] [,2] [,3]
# [1,]   12  0.6 -6.8
# [2,]  -10 -0.6  5.8
# [3,]    1  0.2 -0.6
# > 