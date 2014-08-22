## Function: makeCacheMatrix and cacheSolve calculate
## the invert of a matrix given in argument and store 
## the value in memory so it can be access many times 
## while being calculated only once


## Function: makeCacheMatrix

## This function set the value of m to NULL and
## implement 3 functions
## get           : Fetch the value of the argument given to the function
## setInvMatrix  : Store the value of the inverted matrix in variable m by superassignement
## getInvMatrix  : Fetch the value of variable m

## the function returns a list of the 3 functions described above

## All 3 function are run when a call is done to function cacheSolve


makeCacheMatrix <- function(x = matrix()) {

  m <- NULL

  get <- function() {x}
  setInvMatrix <- function(InvMatrix) {m <<- InvMatrix}
  getInvMatrix <- function() {m}
  list(get = get,
       setInvMatrix = setInvMatrix,
       getInvMatrix = getInvMatrix)
}


## function: cacheSolve

## On the first call this function:
## 1 - calculate the invert of the matrix given in argument 
## 2 - Call the function setInvMatrix of the function  makeCacheMatrixthe  
##     to stores the value of the inverted matrix in memory using superassignement

## On subsequent call this function: 
## Fetch the stored value of the inverted matrix 

cacheSolve <- function(x, ...) {

  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getInvMatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInvMatrix(m)
  m
}
