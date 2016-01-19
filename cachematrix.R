## 
# These functions are provided to efficiently calculate the 
# inverse of a square matrix becasue if the matrix has already been calculated 
# the cached result is displayed rather than calculating the same inverse
# matrix each time.The functions were modelled on similar ones provided 
# by RD Peng, from the John Hopkins Bloomberg School of Public Health.
# *** Example runs are shown below the function definitions.  
#
# Assumptions are that there is a square, invertible matrice
# provided as input so, no validation is performed. 
#
# The purpose of the makeCacheMatrix function is to return 
# a list containing functions and, variables which 
# are then used in  getting (reading) and, setting (assigning) 
# matrix variables. 
#
# Argument: x is the matrix that is passed.
# If no data is passed by the calling routine, The matrix is defaulted
# with a 1 row, 1 column matrix with a single 'NA' value cell.
# 
makeCacheMatrix <- function(x = matrix()) {
                  
    m <- matrix()   
    set <- function(y) {
      x <<- y
      m <<- matrix()
    }   
    get <- function() x
    setmatrix <- function(matrix) m <<- matrix  
    getmatrix <- function() m  
    list(set = set, 
         get = get,
         setmatrix = setmatrix, 
         getmatrix = getmatrix)
}

# Purpose of this function is to return the inverse of a matrix.
# The matrix is either calculated or returned from cache.
#
cacheSolve <- function(x, ...) {
  m <- x$getmatrix()
  if(!is.na(m[1,1])) {
    message("getting cached data.") 
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setmatrix(m)
  m
}
#
#
# Example runs 
# ------------
#
# Run #1
#
# > m <- matrix(c(-1, -2, 1, 1), 2,2)
# > x <- makeCacheMatrix(m)
#
# Showing the 'special' vector which is a list object.
# The enclosing environment of the function holds the variables
# 'm' and 'x' that are used by the functions in the list object (see below).
#
# > ls.str(environment(x$get))
# get : function ()  
# getmatrix : function ()  
# m :  logi [1, 1] NA
# set : function (y)  
# setmatrix : function (matrix)  
# x :  num [1:2, 1:2] -1 -2 1 1
#
# The matrix before being inverted.
# > x$get()
# [,1] [,2]
# [1,]   -1    1
# [2,]   -2    1
#
# The inverted matrix (see below).
# > cacheSolve(x)
# [,1] [,2]
# [1,]    1   -1
# [2,]    2   -1
#
# Another call to invert the matrix and, 
# the cached inverted matrix is used.
#
# > cacheSolve(x)
# getting cached data.
# [,1] [,2]
# [1,]    1   -1
# [2,]    2   -1
#
# Run #2
#
# A different square, invertible matrix is used in this run.
#
# > m <- matrix(c(-1, -2, 1, 3), 2,2)
# > x <- makeCacheMatrix(m)
#
# > ls.str(environment(x$get))
# get : function ()  
# getmatrix : function ()  
# m :  num [1:2, 1:2] -3 -2 1 1
# set : function (y)  
# setmatrix : function (matrix)  
# x :  num [1:2, 1:2] -1 -2 1 3
#  
# > x$get()
# [,1] [,2]
# [1,]   -1    1
# [2,]   -2    3
# > cacheSolve(x)
# [,1] [,2]
# [1,]   -3    1
# [2,]   -2    1
# > cacheSolve(x)
# getting cached data.
# [,1] [,2]
# [1,]   -3    1
# [2,]   -2    1