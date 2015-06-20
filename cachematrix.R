## This code was submitted for Coursera course "R Programming" June 2015, programming
## assignment 2. Based on sample code provided for the assignment.
##
## This code provides a pair of functions which allow cached solving of matrix 
## inverses. The matrix in in the container can be replaced, which will automatically
## clear the cache (set to NULL).
##
## Usage:
## my_CachedMatrix <- makeCacheMatrix(matrix)
## cacheSolve(my_CachedMatrix)
##



## This function creates a matrix container object with a storage location and methods for
## its inverse. It should be used with the below cacheSolve function to provide cached solving.
## Setting hte matrix after initial instantiation will automatically reset the cache.
##
## Usage:
## my_CachedMatrix <- makeCacheMatrix(matrix)
##
## my_CachedMatrix$set(matrix)
## my_CachedMatrix$get()
##
makeCacheMatrix <- function(x = matrix()) {
  
  #cache variable for storge via $setinverse()
  resultcache<- NULL
  
  #setter/getter for the matrix iteself.
  #Setter resets the cache.
  set<-function(y){
        x<<-y
        resultcache<<-null
  }
  get <- function() x

  #getter/setter for matrix inverse calculation result
  #Note that 
  setinverse <- function(inverse) resultcache<<-inverse
  getinverse <- function() resultcache
  
  #return list object holding the above functions
  list(
        set=set,
        get=get,
        setinverse=setinverse,
        getinverse=getinverse
  )
}



## This fucntion takes a matrix container object created with the above makeMatrix()
## function and computes and returns the inverse of the contained matrix in a cached 
## manner. 
##
## Usage:
## cacheSolve(my_CachedMatrix)
##
cacheSolve <- function(x, ...) {
  
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        
        #if x does not have a cached solution generate it
        if(is.null(inverse)) {
          data <- x$get()
          inverse <- solve(data, ...)
          x$setinverse(inverse)
        }
        
        return(inverse)
}
