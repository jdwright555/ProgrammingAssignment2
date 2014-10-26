## The purpose of the following two functions is to allow the user to cache the inverse of a 
## matrix in memory; saving on computational power by removing the need to re-calculate. The 
## first of these functions, makeCacheMatrix, takes the matrix of interest as its object. It 
## then creates a list, in the global environment, where each of the elements is another function.
## These secondary functions are used to set and call values of the matrix itself and its inverse 
## and are invoked by the second of the primary funcions, cacheSolve. It is cacheSolve that 
## actually finds the inverse of the original matrix. When it does this it invokes one of the 
## functions set up by makeCacheMatrix to save the inverse as a local value within the environment 
## of makeCacheMatrix (that is, away from the global environment - in a cache). Before doing this, 
## cacheSolve scans the cache (the local environment of makeCacheMatrix) for a non-null value. If
## it finds that the inverse is already stored within the cache, it simply returns it, rather than
## re-calculating.  

makeCacheMatrix <- function(x = matrix()) {    ## makeCacheMatrix takes the matrix of interest as input and refers to it as x
  
  i <- NULL                                    ## the inverse, i, is initally assigned to NULL
  
  set <- function(y) {                         ## the first of the four functions created, set can be used to overwrite the input matrix
    x <<- y                                    ## the superassigment operator, <<-, escapes the envrionment of set and overwrites x with the new matrix y
    i <<- NULL                                 ## this line overwrites i, ensuring that i is reset to NULL if a new matrix is entered
  }
  
  get <- function() x                          ## the get funtion allows the user (or cacheSolve) to call the matrix x
  
  setinv <- function(inv) i <<- inv            ## if passed a new value of i from cacheSolve, the setinv function will overwrite it within the environment of makeCacheMatrix (the cache)
  
  getinv <- function() i                       ## the getinv function allows cacheSolve to call the value of i, and scan the cache
  
  list(set = set, get = get,                   ## finally, each of our four functions is assigned to the elements of a list;
       setinv = setinv,                        ## the list exists in the global environment, allowing cacheSolve to invoke them.
       getinv = getinv)

}


## cacheSolve does the actual calculation, interacting with the functions in the list created by
## makeCacheMatrix

cacheSolve <- function(x, ...) {               ## again, cacheSolve takes the matrix of interest as its input
  
  i <- x$getinv()                              ## first, cacheSolve invokes the getinv function to scan the cache for a value of the inverse
  if(!is.null(i)) {                            ## if a non-null value is found...
    message("getting cached data")             ## we get a cheeky message...
    return(i)                                  ## and the value of i is simply returned...
  }              
                                               ## if the cache is empty (perhaps because a new matrix has just been defined)...
  data <- x$get()                              ## cacheSolve uses the get function to call in the matrix...
  i <- solve(data, ...)                        ## and calculates its inverse.
  x$setinv(i)                                  ## the original, null value of i is overwritten in the environment of makeCacheMatrix (the cache) using the setinv function
  i                                            ## finally, this value is returned.
}
