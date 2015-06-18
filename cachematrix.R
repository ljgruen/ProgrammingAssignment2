## This program contains two functions which cache the value of
## the inverse of a matrix so that when we need it again, it can
## be looked up in the cache rather than computed


## makeCacheMatrix is a function that stores a list of 4 functions:
## set, get, setinverse, getinverse that changes the matrix stored
## in the main function, gets the matrix stored, store the value of 
## the input in a variable m into the main function and return it
## respectively

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setinverse<-function(inverse) m<<-inverse
  getinverse<-function() m
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## cacheSolve verifies the value m stored previously with
## get inverse and that it exists and is not null. If it
## exists in memory, it returns a message and the value m
## that is supposed to be the inverse, but not necessarily
## Data gets the matrix stored with makeCacheMatrix, m
## calculates the inverse of the matrix and x$setinverse(m)
## stores it in the object generated assigned w/ makeCacheMatrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m<-x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data,...)
  x$setinverse(m)
  m
}