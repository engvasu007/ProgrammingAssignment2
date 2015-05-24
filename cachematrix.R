## Our aim is to reduce the repeated computations over same data. Here we want to cache the inverse of a matrix, 
## so that if the inverse of same matrix is needed again we can get it from the cache without recomputing

## For this purpose, here we have impleted two functions to create a matrix, cache the inverse value of matrix and to 
## retrieve it back when needed

## This function (makCacheMatrix) returns a lsit of four functions.
## set <- to set the values of a matrix
## get <- to get the matrix
## setInv <- to store the value of Inverse
## getInv <- to get the stored Inverse

makeCacheMatrix <- function(x = matrix()) {
  I <- NULL     #Initializing Inverse as NULL
  
  # Set the value of Matrix & Inverse
    set <- function(y) {   
    x <<- y
    I <<- NULL
  }
  
  # Get the value of Matrix
  get <- function() x
  
  # function to store Inverse value in cache
  setInv <- function(Inv) I <<- Inv
  
  # function to pass the cached Inverse of Matrix
  getInv <- function() I           
  
  #Returning a list of functions
  list(set = set, get = get,       
       setInv = setInv,
       getInv = getInv)
}


## This function(cacheSolve) will first check if the inverse already exists in the cache and skips the computations.
## if not it will find the inverse for it and stores it in the cache using 'setInv' function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  #Gets the Inverse of matrix if it exists
  I <- x$getInv()
  
  # Checking if Inverse is available in cache
  if(!is.null(I)) {   
    message("getting cached data")
    return(I)
  }
  
  #Get the value of matrix
  data <- x$get() 
  
  #Solving for Inverse of a matrix
  I <- solve(data, ...)
  
  #Storing Inverse of matrix
  x$setInv(I)
  
  #Returning the Inverse
  I
        
}
