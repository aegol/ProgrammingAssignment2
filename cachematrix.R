## The below are a pair of functions that cache the inverse of a given matrix

## makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<-inverse
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}

## cacheSolve (below) computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above.
## (it takes as its argument, the list returned by makeCacheMatrix)
## If the inverse has already been calculated (and the matrix has not changed), 
## then the it retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        ## compute a matrix that is the inverse of 'x'
        matrix1 <- x$get()
        i <- solve(matrix1, ...)
        x$setinverse(i)
        return(i)
}
