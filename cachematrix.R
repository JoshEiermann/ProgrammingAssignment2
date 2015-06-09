# These functions will cache the inverse of a matrix, which 
# will be faster than computing the multiple times

# makeCacheMatrix is a list of functions to: 
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse
# 4. Get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
        # will store the cached inverse matrix
        inv <- NULL
        
        # 1. Set the value of the matrix
        # 2. Get the value of the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        
        # 3. Set the value of the inverse
        # 4. Get the value of the inverse
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


# This function will return an inverse of the matrix. It checks if
# the inverse has been computed, and if it has, it gets the result
# and does not compute. If it hasn't, then it computes the inverse, 
# and caches the value. 

cacheSolve <- function(x, ...) { inv <- x$getinv()
        # Returns the inverse if it is calculated 
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }

        # Calculate the inverse since it is not yet calculated
        # then set and cache the inverse, and return it. 
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}

# Test the functions
x = cbind(c(1, 3.5), c(3.5, 1))
m = makeCacheMatrix(x)
m$get()
# First, will not have a cache
cacheSolve(m)
# Second, will retrieve the cache with the message 
cacheSolve(m)