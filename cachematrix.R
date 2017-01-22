## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    ## To use this function, first initialize it by "a <- makeCacheMatrix()"
    ## 'a' is a list of four functions: set, get, setsolve, getsolve
    ## Suppose 't' is a matrix and 't2' is its inverse, 
    ## e.g., t = matrix(c(1,0,2,1),2,2) and t2 = solve(t)
    ## Type "a$set(t)" to store the matrix 't' in 'a'
    ## Type "a$get()" to get the stored matrix
    ## Type "a$getsolve()" to get the cached inversed matrix
    ## To cache the inversed matrix, there are two ways:
    ##   First way is to type "a$setsolve(t2)"
    ##   Second way is to use the cacheSolve function: "cacheSolve(a)"
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## If the inverse of 'x' is already cached in 'x', output it
    ## If not, compute the inverse, cache it in 'x', and output it
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setsolve(m)
    m
}
