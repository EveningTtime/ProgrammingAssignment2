## JHU Coursera Programming Assignment #2
## August 2020
## author: Khrysna T.P.
##
## Invert a matrix and cache its result such that 
## requests for matrix inversion is retrieved rather than recalculation.


## makeCacheMatrix, creates a function that stores matrix X and caches its inverse.
## In this assignment, the matrix is assumed to be invertible.
## This function returns 4 sub function :
## $set(Y) is used to set the value of matrix X within the function.
## $get() returns the matrix of value stored within the function.
## $setinv(invval) is used to set the value of inverse matrix cached within the function.
## $getinv() returns the cached inverse matrix of X (may be NULL or a value).

makeCacheMatrix <- function(X = matrix()) {
  ##  Set initial value of matriX inverse to NULL
  matinv <- NULL 
  
  
  ## $set(Y), changes value of matrix X within function into matrix Y.
  set <- function(Y) { 
    X <<- Y
    matinv <<- NULL # Change matinv to NULL from its previous value.
  }
  
  
  ## $get(), Returns the numerical value of matrix X
  get <- function() X
  
  
  ## setinv(invval), changes cached inverse value to invval.
  setinv <- function(invval) matinv <<- invval
  
  
  ## $getinv(), Returns the numerical value of matinv.
  getinv <- function() matinv # Function to return the value of inverse of X
  
  
  ## Returns list of four functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}  



## cacheSolve, returns inverse matrix of X stored within list of functions DATA
## DATA contains list of functions $set(Y),$get(),$setinv(invval),$getinv()
## as described in makeCacheMatrix.
## ... are optional arguments for solve() function used within cacheSolve.

cacheSolve <- function(DATA, ...) {
        
  
        ## Call the cached value of inverse within DATA and assign it to matinv.
        matinv <- DATA$getinv() 
        
        
        ## Check wheter the inverse value in cache is NULL
        ## if cache is available, return the stored value.
        if(!is.null(matinv)) {
          
          message("returning cached inverse...")
          
          # Return the value stored inverse in the cache and exit the function.
          return(matinv) 
        }
        
        
        ## Call the value of matrix X within DATA and assign it to M.
        M <- DATA$get() 
        
        
        ## Obtain the dimension of invertible matrix M.
        ## note invertible matrices are nxn, hence ncol=nrow.
        dim_n <- ncol(M) 
        
        
        ## Solve for inverse of matrix M using solve() function.
        ## diag(dim_n) is the identity matrix with dim_n x dim_n dimension.
        matinv <- solve(M,diag(dim_n),...) 
        
        
        ## Call setinv within DATA
        ## and set the value of matinv (within its own space in the function).
        DATA$setinv(matinv) 
        
        
        ## Return a matrix that is the inverse of 'X' stored within DATA
        matinv
}
