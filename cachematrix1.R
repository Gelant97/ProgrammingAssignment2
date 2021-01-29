## Put comments here that give an overall description of what your
## functions do

#This function is made to store objects in memory. This method can reduce
#the time needed for calculating particular data by recalling the previous
#calculation (instead of recalculating from the beginning).In this case, matrix 
#data is stored to the parent environment and being inversed.

## Write a short comment describing this function

#makeCacheMatrix() builds a set of functions and returns it within a list to the
#parent environment.

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function()x
  setsolve<- function(solve) m <<- solve
  getsolve<-function() m
  list(set = set, get = get,
       setsolve = setsolve, 
       getsolve = getsolve)
}


## Write a short comment describing this function

# cacheSolve() is needed to complete the function because the inverse value is
#to retrieve from an object of type makeCacheMatrix()

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m<- x$getsolve()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m<- solve(data, ...)
  x$setsolve(m)
  m
}

