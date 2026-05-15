setwd("C:/Users/user/Desktop/R_Assignment")

## This program utilizes lexical scoping creates a matrix object
##that caches its inverse.

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL ##Initializes inv as null in function environment
  set <- function(y){ ##A function that is used to update matrix
    x <<- y ## Updates x in parent environment
    inv <<- NULL
  }
    get <- function() x ## Returns matrix x
    setInverse <- function(inverse) inv <<- inverse ##Assigns calculated value to cache
    getInverse <- function() inv ##Returns cache value of inv
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
    ##List that can be accessed using $ operator
}

## This function retrieves a matrix from cache if calculated, else 
##calculate same using solve()
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)){
    message("fetching data from cache")
    return (inv)
  }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv ## Return a matrix that is the inverse of 'x'
}
my_matrix <- makeCacheMatrix(matrix(c(4,3,3,2),2,2))
cacheSolve(my_matrix)
cacheSolve(my_matrix)


