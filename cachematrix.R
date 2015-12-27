## Assignment 2 - Caching the Inverse of a Matrix
## Notes:
##   clarification https://github.com/DanieleP/PA2-clarifying_instructions
##   <<- assigns globally

## makeCacheMatrix() creates special "matrix" object that can cache its inverse
##    1) set the matrix values
##    2) get the matrix
##    3) set the values of inverse
##    4) get the values of inverse
##       this list is used as input for cacheSolve()


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- null
        }
        get <- function() x
        setInv <- function(inverse) m <<- inverse
        getInv <- function() m
        list(set = set, get = get, 
             setInv = setInv,
             getInv = getInv)

}


## cacheSolve() computes the inverse of the makceCacheMatrix list. 
##              if the inverse already calculed & matrix not changed
##              then retrieve inverse in cache
##    1) get inverse matrix list
##    2) if already calculated, skip calculations and pull in cache data
##    3) if not already calculated, run calculation

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInv()
        if(!is.null(m)) {
                message("getting cached inverse data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInv(m)
        m
}

## unit test
