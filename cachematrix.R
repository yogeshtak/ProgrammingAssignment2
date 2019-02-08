## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {       #set value in matrix
                x <<- y
                inv <<- NULL
        }
        get <- function() x               #get the value of matrix
        setinv <- function(inverse) inv <<- inverse      #setting the value
        getinv <- function() inv                               #getting the value
        list(set = set, get = get, setinv = setinv,  getinv = getinv)   #list containing all the functions
        
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

# cacheSolve function will calculate the inverse of the matrix created in the makeCacheMatrix function 
# firstly the function checks if the inverse was calculated before. 
# If this is the case, the inverse is retractecd from the cache.
# If the inverse isn't  calculated before, the cacheSolve function will calculate the inverse of the data and sets the value of the inverse in the cache.
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
