## These two functions shall save computation time 
# when repeatedly inverting the same matrix 
# by caching the inverse.
# Steps:
# 1) Take a square matrix, 
# 2) Create a list of get/set functions for the matrix and its inverse,
# 3) Calculate the inverse IFF it is not already stored in the list.
# This is an example of object-oriented programming. 

## The first function, makeCacheMatrix creates a special "matrix object", 
# a list of functions that
# set the value of the matrix,
# get the value of the matrix,
# set the value of the inverse,
# get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) { # input: a matrix; default: empty
      invx <- NULL # initialize what will be the inverse of x, 'invx',
      # with a NULL value
      set <- function(y) { 
            # create function 'set': 
            # 1) takes value y, assigns it to x (not neccessary for example, 
            # only if repeatedly calling makeCacheMatrix with different matrices) 
            # 2) re-assign 'invx' the value NULL when changing 'x'. 
            x <<- y # operator '<<-' overwrites x in the global environment
            invx <<- NULL
      }
      get <- function() x # create function 'get': 
      # takes no inputs and gives x as output 
      setinv <- function(inverse) invx <<- inverse 
      # create function 'setinv': takes value 'inverse' and assigns it to 'invx'
      # operator '<<-' overwrites 'invx' within the global environment
      getinv <- function() invx 
      # create function 'getinv': takes no inputs and outputs 'invx'
      list(set = set, get = get, # output: a list of functions associated with x
           setinv = setinv,
           getinv = getinv)
}

## The function 'cacheSolve' calculates the inverse of the matrix 
# associated with the 'matrix object' 
# created with the above function'makeCacheMatrix'. 
# However, it first checks to see if the inverse has already been calculated. 
# If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse  
# and sets the value of the inverse in the cache via the setmean function.

cacheSolve <- function(x, ...) {
      # Return a matrix that is the inverse of 'x'
      # Arguments: x is the special 'matrix object' created by makeCacheMatrix 
      # '...' stands for everything else (?)
      invx <- x$getinv() # assign the inverse from the matrix object to invx
      if(!is.null(invx)) { # if that is NOT NULL, return the value and be happy. 
            message("saving you time - getting cached data - you are welcome")
            return(invx) # return value 'invx' and stop function execution.
      }
      data <- x$get() # if invx' is NULL, assign original matrix to 'data'
      invx <- solve(data, ...) # calculate inverse of data - (Why the '...'?)
      x$setinv(invx) # call 'setinv' function to assign inverse to invx
      invx
      
}
