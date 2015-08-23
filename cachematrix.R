## Functions below give an opportunity to cache the inverse of a matrix

## This function creates a special "matrix", 
## which is actually a list containing 4 functions:
## 1. to set the matrix
## 2. to get the matrix
## 3. to set the inversed matrix
## 4. to get the inversed matrix
makeCacheMatrix <- function(x = matrix()) {
        xinv <- NULL
        setMatrix <- function(y) {
                x <<- y
                xinv <<- NULL
        }
        getMatrix <- function() x
        setInversedMatrix <- function(yinv) xinv <<- yinv
        getInversedMatrix <- function() xinv
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInversedMatrix = setInversedMatrix,
             getInversedMatrix = getInversedMatrix)
}


## Function cacheSolve calculates the inverse of the special "matrix" created by function makeCacheMatrix
## First it checks to see if the inverse has already been calculated and stored in cash.
## If yes, it skips calculations and returns cached value.
## If not, it calculates the inverse of the matrix, stores it in cache and returns it.
cacheSolve <- function(y, ...) {
        xinv <- y$getInversedMatrix()
        if(!is.null(xinv)) {
                message("getting cached data")
                return(xinv)
        }
        x <- y$getMatrix()
        xinv <- solve(x, ...)
        y$setInversedMatrix(xinv)
        xinv
}
