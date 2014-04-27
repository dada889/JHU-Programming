## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {                            #set value of the matrix
                x <<- y
                m <<- NULL
        }    
        get <- function() x                             #get value of the matirx
        setinverse <- function(inverse) m <<- inverse   #set value of the inverse of the matrix
        getinverse <- function() m                      #get value of the inverse of the matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)                   #list the result
}


## Write a short comment describing this function
## This function calculates the inverse of the matrix created with above function. It checks if the inverse has been calculated. if so, get the inverse from the cache and skips the computation. Otherwise, it calculates the inverse and set the value in the cache via setinverse function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()                       #set the inverse in m
        if(!is.null(m)) {                         #check if the inverse has been calculated, if so, get the inverse from the cache and skips the computation
                message("getting cached data")
                return(m)
        }
        data <- x$get()                           #if m is NULL
        m <- solve(data, ...)                     #calculate the inverse 
        x$setinverse(m)                           #set the value of inverse in the cache vua the setinverse function
        m                                         #return the inverse
}
