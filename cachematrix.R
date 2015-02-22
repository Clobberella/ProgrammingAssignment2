##     Cache and calculate the inverse of a matrix by writing 2 functions.

##     This first function creates a special matrix object.
##     1. set the value of the matrix
##     2. get the value of the matrix
##     3. set the value of the inverse matrix
##     4. get the value of the inverse matrix

makeCacheMatrix<- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(calc) m <<- calc
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


##      This second function calculates the inverse of the matrix first by
##      checking to see if the inverse has been computed. If yes, it finds the value 
##      in the cache and returns it, skipping the computation. If no, it solves the 
##      inverse and sets the value in cache through the setmatrix function.

cacheSolve <- function(x=matrix, ...) {
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setmatrix(m)
        m
}

##      Test Run:
##      a<-makeCacheMatrix()
##      a$set(matrix(6:9,2,2))
##      cacheSolve(a)