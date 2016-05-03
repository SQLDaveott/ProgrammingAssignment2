##
##    makeCacheMatrix will 
##        :store the inverse of matrix into a different environment 
##        :retrieve the stored inverse matrix from the environment 
##


## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## initialize variable 
  im <- NULL
  
  ## save matrix and Null out inverse, assume there was a change
  set<-function(y){
      x<<-y
      im <<-NULL
  }
  ## get Maretix 
  get <- function() x
  ## set/store inverse
  setInverse <- function(imax) im <<- imax
  ## get/retreive inverse
  getInverse <- function() im
  # return function names
  list(set = set, get= get,
       setInverse = setInverse,
       getInverse = getInverse)

}


##    cacheSolve will call makeCacheMatrix:getInverse to retreive the matrix stored
##      If the matrix has not been save it will calculate the inverse
##      and then call makeCacheMatrix:setInverse to store teh natrix inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      
    ## get what is saved, matrix and inverse
    m<- x$get()  
    im <- x$getInverse()
    
    ## if we have a valid inverse return with it
    if(is.null((im))) {
      message("getting cached data")
         return(im)
    }
    
    ## We have no inverse stored 
    ##  or the matrix stored is not the same passed in
    ##  calculate inverse, save inverse and return inverse
    message("Calculating data")
    m<-x$get()
    im <- solve(m,...)
    x$setInverse(im)
    im
}
