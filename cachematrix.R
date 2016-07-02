## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Rather than perform expensive compute operations repeatedly on data it
## is preferable to cache the results of prior calculations and reuse
## this data in the future.
##
## These functions are an example of caching data from a calculation.
##
## The first function (makeCacheMatrix) is effectively a Closure. The 
## function is assigned to a variable and then the sub-functions:
##   - set()
##   - get()
##   - setmatrix()
##   - getmatrix()
## are made available through the variable.
## For example - 
## x <- makeCachematrix(m) (where 'm' is a matrix) would see x as a
## "freestanding object" (my words) that a) holds the matrix data and
## b) possesses the 4 sub-functions.

makeCacheMatrix <- function(x = matrix()) {
    # Create a variable to hold the cached matrix
    invMatrix <- NULL
    
    # Setter function
    # Assign a value to x and set the inverse to NULL
    # Because you're setting x to a new value, you want to reset
    # the inverse matrix (invMatrix) to NULL because it hasn't 
    # been calculated yet.
    #
    # This is a sub-function so you need to use <<- notation to
    # reference the invMatrix variable defined above.
    set <- function(y) {
        x <<- y 
        invMatrix <<- NULL
    }
    
    # Getter Function
    # Return the value of the original matrix
    get <- function() {
        x
    }
    
    # Setter Function
    # 
    setInvMatrix <- function(solve) {
        invMatrix <<- solve
    }
    
    # 
    getInvMatrix <- function() {
        invMatrix
    }
    
    # here to reference in the future once I work out what it does.
    list(set          = set,
         get          = get,
         setInvMatrix = setInvMatrix,
         getInvMatrix = getInvMatrix)
}

## The function checks for the existance of an existing inverse matrix.
## If found it simply prints a message (Getting cached data) and then
## returns the cached data
##
## If there is no existing cached data then it uses solve() to first
## calculate the inverse and then set it in the makeCacheMatrix closure

cacheSolve <- function(x, ...) {
    ## Try to retrieve an existing cached matrix
    invMatrix <- x$getInvMatrix()
    
    ## If the value is not null (data exists) 
    ## 1. Print "Getting cached data" message
    ## 2. Return the cached data
    if(!is.null(invMatrix)) {
        message("Getting cached data")
        return(invMatrix)
    }
    
    ## If there was no cached data then:
    ## 1. solve()
    ## 2. Set in the closure
    ## 3. Return the inversed matrix
    invMatrix <- solve(x$get())
    x$setInvMatrix(invMatrix)
    invMatrix
}

## Testing
## Make a matrix(10 x 10) of random variables
set.seed(276729)
mat1 <- matrix(rnorm(100), nrow=10, ncol=10)

##
testMatrix <- makeCacheMatrix(mat1)
cacheSolve(testMatrix)

