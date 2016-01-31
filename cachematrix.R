## makeCacheMatrix will create a matrix x, and expose three methods to set/get x and its inverse

makeCacheMatrix <- function(x = matrix()) {
  ## initialize inverse
  cachedInv <- NULL
  
  ## set x in parent env with the desired value, if inverse is already set, unload cachedInv value
  set <- function(userValue = matrix()) {
    x <<- userValue 
    cachedInv <<- NULL
  }
  
  get <- function() x
  
  ##set inverse variable in desired value and return the value
  setInverse <- function(invVal) {
    cachedInv <<- invVal 
    return(cachedInv)
  }
  
  getInverse  <- function() cachedInv
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

## first check to see if a cached inverse value already exists, if so - return the value
## else, solve for its inverse and set the new value and return it

cacheSolve <- function(x=makeCacheMatrix(1:4, nrow=2, ncol=2), ...) {
  
  ## check to see if value exists already
  calculatedInverse <- x$getInverse() 
  
  ## check if cached value exists and if it is a matrix
  if(!is.null(calculatedInverse) && is.matrix(calculatedInverse)) { 
    message("Cached Value Exists")
    return(calculatedInverse)
  }
  
  ## else - get the matrix value
  matrixToSolve <- x$get()  
  
  ## try to solve the matrix and catch errors and warnings
  calculatedInverse <- tryCatch({ 
    solve(matrixToSolve)
  }, warning=function(w) {
    message("Warning: result may not be as you are expecting")
    message(w)
  }, error=function(e) {
    message("Error occurred in solving your matrix")
    message(e)
    message("\n")
  })
  
  ## catchall for setting the value of the inverse in event something went wrong
  message("Setting value of inverse to:") 
  x$setInverse(calculatedInverse)
}
