## This program has been written to calculate the inverse of a matrix and cache the
## output rather than computing it repeatedly as it is a costly computation

##makeCacheMatrix is a function which creates a special matrix object which can cache the inverse of a matrix 

makeCacheMatrix <- function(x = matrix())
{
  
    i<-NULL
    
    set<-function(y)
      {
        x<<-y
        i<<-NULL
      }
  
    get<-function()
      {
        x
      }
  
    setinverse<-function(inverse)
      {
        i<<-inverse
      }
  
    getinverse<-function()
      {
        i
      }
    
    list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  

}


## cacheSolve is a function which calculates the inverse of the matrix returned by makeCacheMatrix
##If the inverse is already calculated (and the matrix has not changed) then cacheSolve retrieves the value of the inverse from the Cache

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
    i<- x$getinverse()
  
    if(!is.null(i))
      {
        message("getting cached data")
        return(i)
      }
  
    mydata<-x$get()
    
    i<-solve(mydata)
    
    x$setinverse(i)
    
    i

}