## This is the R script for R Programming - Week 3
## Programming assignment 2 - Lexical Scoping

## Author: Giulia Cauli
## giulia.cauli@studium.uni-hamburg.de
## Version 20200906, Sept. 6th 2020

## The following functions allow to cache potentially time-consuming
## computations. We have a matrix, whose contents won't change, so we can cache
## its inverse and use it whenever we need it, instead of recomputing it.

## The first function creates a matrix object that will cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv_matrix <- NULL
  set <- function(y) {
    x <<- y
    inv_matrix <<- NULL
  }
  get <- function() x
  set_inverse <- function(solve) inv_matrix <<- solve
  get_inverse <- function() inv_matrix
  list(set = set, get = get, set_inverse = set_inverse,
       get_inverse = get_inverse)
}

## The second function computes the inverse of the matrix created in the first
## function. If the inverse has already beel computed, then there is no need for
## any further computation and the inverse matrix will be simply retrieved from
## the cache in the first function.

cacheSolve <- function(x, ...) {
  inv_matrix <- x$get_inverse()
  if(!is.null(inv_matrix)) {
    message("getting cached matrix")
    return(inv_matrix)
  }
  matrix <- x$get()
  inv_matrix <- solve(matrix, ...)
  x$set_inverse(inv_matrix)
  inv_matrix
}
