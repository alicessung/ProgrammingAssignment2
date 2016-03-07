## Coursera R Programming
## Week 3
## Programming Assignment 2

# This function creates a special "matrix" object that can cache its inverse.
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setinvs <- function(invs) s <<- invs
    getinvs <- function() s
    list(set = set, get = get,
         setinvs = setinvs,
         getinvs = getinvs)
}

# This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already 
# been calculated (and the matrix has not changed), then cacheSolve 
# should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    
        ## Return a matrix that is the inverse of 'x'
    s <- x$getinvs()
    if(!is.null(s)) {
        message("getting cache inverse")
        return(s)
    }
    matrix <- x$get()
    s <- solve(matrix)
    x$setinvs(s)
    s
}
