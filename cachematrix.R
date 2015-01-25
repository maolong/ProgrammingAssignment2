## Put comments here that give an overall description of what your functions do
# There are two functions inside this program: one creates a special matrix (contains a matrix and its inverse in cache); the other resolve the inverse of a matrix using cached if exists

## Write a short comment describing this function
# makeCacheMatrix creates a list that contains
# A. function to set the value of the matrix
# B. function to get the value of the matrix
# C. function to set the value of inverse of the matrix
# D. function to get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        mat <- NULL
        set <- function(y) {
                x <<- y
                mat <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) mat <<- inv
        getinverse <- function() mat
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
# This function assumes that the matrix is always invertible.
# It returns the inverse of the matrix. 
# The inverse is retrieved from cache if already calculated. The IF checks if cached inverse exists.
# If inverse doesn't exist in cache then the fuction computes the inverse and sets the value in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mat <- x$getinverse()
        if(!is.null(mat)) {
                message("getting cached data")
       	        return(mat)
        }
        data <- x$get()
        mat <- solve(data)
	x$setinverse(mat)
        mat
}

# Test
# 
# > z <- matrix (1:4,2,2)
# > z
#      [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > k <- makeCacheMatrix(z)
# > k$get()
#      [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > cacheSolve(k)
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(k)
# getting cached data
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > 
# 