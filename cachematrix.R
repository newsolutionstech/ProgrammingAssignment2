
## To start, makeCacheMatrix() is passed a matrix which creates an
## "object" of type 'list'.  This object stores two things, the
## original matrix's value and what will be the cached value,
## which is initially set to 'NULL'.  There are four functions,
## two to read (or 'get') the value of the two things we are storing,
## and two functions to change ('set') them.

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL                ## begins by setting a placeholder for a future value to NULL
    
    set <- function(y) {     ## takes an input matrix
      x <<- y                ## saves the input matrix
      m <<- NULL             ## resets the inverse matrix to NULL
    }  ## defines a function to set the matrix, x,  to a new matrix,
       ## y, and resets the inverse matrix, m, to NULL

  get <- function() {x} ## returns the original matrix, x

  setinv <- function(inv) {m <<- inv} ## sets the inverse matrix, m, to source
  
  getinv <- function() {m} ## returns the inverse matrix, m
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

 ## cacheSolve() accesses the object (not the makeCacheMatrix() function,
 ## but the object created when makeCacheMatrix() was called)
 ## by fetching the value of the matrix used to create the object,
 ## this matrix being stored when the object was created.

 ## If the inverse has not yet been calculated (if it is still is 'NULL')
 ## cacheSolve() calculates the inverse and stores it in the object
 ## created by the call to makeCacheMatrix(), then returns the inverse.
 ## If the inverse has been calculated earlier then cacheSolve() simply
 ## fetches it and returns the inverse value, saving the computing time
 ## required to calculate the inverse again. Either way cacheSolve()
 ## returns the inverse.

cacheSolve <- function(x, ...) { ## the input x is an object object created by makeCacheMatrix
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinv()  ## access the object 'x' and gets the value of the mean

  if(!is.null(m)) { ## if mean was already cached (not NULL)
    
    message("getting cached data") ## ...send this message to console
    
    return(m)                      ## ...and return the inverse ... "return" ends
  
    }                              ## the function cacheSolve(), note
  
  data <- x$get() ## we reach this code only if x$getinv() returned NULL

  m <- solve(data, ...) ## if m was NULL then we have to calculate the inverse

  x$setinv(m)  ## store the calculated inverse value in x (see setinv() in makeCacheMatrix)
  m            ## return the inverse to the code that called this function

}

