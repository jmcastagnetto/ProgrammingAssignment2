# This file defines:
# - a function that defines an special "matrix" that can cache its inverse
# - a function that calculates the inverse of the special "matrix"

# Defines a special "matrix" capable of caching its own inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(w) {
        x <<- w
        inv <<- NULL
    }
    get <- function() {
        return(x)
    }
    # cache the inverse
    setinverse <- function(inverse) {
        inv <<- inverse
    }
    getinverse <- function() {
        return(inv)
    }
    list(get=get, set=set,
         setinverse=setinverse,
         getinverse=getinverse)
}


# Returns the inverse of the special "matrix"
# indicating whether the result has been cached
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message("getting the cached value of the inverse of the matrix")
        return(inv)
    } else {
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setinverse(inv)
        return(inv)
    }
}

## Testing the functions defined above

set.seed(13579)

# Let's create an special "matrix" from a 3x3 matrix
mat <- matrix(sample(1:4,9,replace=TRUE), nrow=3, ncol=3)
cmat <- makeCacheMatrix(mat)
invcmat <- cacheSolve(cmat)
# get the cached value
cacheSolve(cmat)
# compare the inverse with the 3x3 unit matrix
all.equal(round(invcmat %*% mat,1), diag(3))

# now, we will use a 100x100 matrix
mat2 <- matrix(sample(1:4, 10000,replace=TRUE), nrow=100, ncol=100)
cmat2 <- makeCacheMatrix(mat2)
invcmat2 <- cacheSolve(cmat2, tol=1e-5)
# get the cached value
tmp <- cacheSolve(cmat2)
# compare with the 100x100 identity matrix
all.equal(round(tmp %*% mat2, 1), diag(100))

