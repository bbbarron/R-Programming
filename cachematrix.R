## bbarron December 19, 2014
## R Programming Assignment 2: Lexical Scoping
## Program to demonstate lexical scoping and the concept of superassignment.
## The makeCacheMatrix takes an initial  matrix and stores (caches) 
## the matrix for future calls to the function with the same input,
## in four steps it:
## 1. sets the value of the matrix
## 2. gets the value of the matrix
## 3. sets the value of the inverse of the matrix
## 4. gets the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    mat_cache <- NULL    ## initialize the empty cache matrix
    set_matrix <- function(new_value) { ## initialize if recalled with new input
        x <<- new_value
        mat_cache <<- NULL
    }
    get_matrix <- function() x       ## retrieve the stored matrix
    set_inverse <- function(inverse) mat_cache<<- inverse ## calculate the inverse of the matrix
    get_inverse <- function() mat_cache
    list(
        set_matrix = set_matrix,
        get_matrix = get_matrix,
        set_inverse = set_inverse,
        get_inverse = get_inverse)
}

## The cacheSolve function retrieves the inverse matrix if not NUL
## otherwise, it calculates it.

cacheSolve <- function(x, ...) {
    ## Return the inverse matrix of x.
    mat_cache <- x$get_inverse()
    if(!is.null(mat_cache)) {
        message("retrieving cached data")
        return(mat_cache)
    }
    data <- x$get_matrix()
    mat_cache <- solve(data, ...)
    x$set_inverse(mat_cache)
    mat_cache
}