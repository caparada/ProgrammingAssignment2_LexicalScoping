# Based on the examples given
# Let's create the 1st function named makeCacheMatrix()
# makeCacheMatrix() creates a special "matrix" and:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse matrix using the R function solve()
# 4. get the value of the inverse matrix using the R function solve()
makeCacheMatrix <- function(x = matrix()) { # it creates a special "matrix"
  inv <- NULL # when makeCacheMatrix() is called, it initializes the variable (R object) inv to NULL
  set <- function(y) { # it sets the value of the matrix
    x <<- y
    inv <<- NULL 
  }
  get <- function() x # it gets the value of the matrix
  setInv <- function(solve) inv <<- solve # it sets the value of the inverse matrix using the R function solve()
  getInv <- function() inv # it gets the value of the inverse matrix using the R function solve()
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

# The object inv will (later) hold the inverse of the matrix x that makeCacheMatrix was called with 

# Making the cacheSolve() function 
# which computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# then the cachesolve() should retrieve the inverse from the cache if it is not NULL.
cacheSolve <- function(x, ...) {
  inv <- x$getInv()  # It first checks if the inverse of the matrix (inv) is already cached using x$getInv().
  if(!is.null(inv)) { # If the inverse exists
    message("getting cached matrix") # this msg is printed
    return(inv) # and cachesolve() retrieves the inverse from the cache.
  }
  data <- x$get() # otherwise it calculates the inverse of the matrix
  inv <- solve(data, ...)
  x$setInv(inv) # and caches it using x$setInv for future use
  inv # and returns the matrix inverse
}


# Testing the functions
# Testing the function makeCacheMatrix()
x <- matrix(1:4, 2, 2) # creating an input
x # check
makeCacheMatrix.object <- makeCacheMatrix(x) #  calling makeCacheMatrix(x) and storing in the var makeCacheMatrix.object
# Using makeCacheMatrix.object to invoke the 4 functions set in makeCacheMatrix():
makeCacheMatrix.object$set
makeCacheMatrix.object$get
makeCacheMatrix.object$setInv
makeCacheMatrix.object$getInv

# Testing the cacheSolve() function
cacheSolve(makeCacheMatrix.object)
