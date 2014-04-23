## Script contains 2 functions makeCacheMatrix and cacheSolve
## makeCacheMatrix makes a matrix, cacheSolve calculates the inverse of that matrix


## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)  
}


## cacheSolve:  This function computes the inverse of the special "matrix" 
##              returned by makeCacheMatrix above. If the inverse has already 
##              been calculated (and the matrix has not changed), then the cachesolve 
##              should retrieve the inverse from the cache. The script then shows 
##              the message: "getting cached data" followed by the result of the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data) #, ...)
        x$setinverse(m)
        m 
}

## To test the functions do this:
## step 1 (without commentmarks):
## x <- makeCacheMatrix(matrix(1:4, 2, 2)) ## make a matrix of 2 rows and 2 columns containting the values 1, 2, 3, 4
## step 2 (without commentmarks):
## cacheSolve(x) ## returns the inverse of the matrix above
## step 3 (without commentmarks):
## cacheSolve(x) ## returns the same result as step 2, but without calculations (getting cached data)
