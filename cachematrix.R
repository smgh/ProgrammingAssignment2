##  makeCacheMatrix and cacheSolve functions work in tandem to calculate, store and return 
##  cached or computed inverse of a square matrix returned by makeCacheMatrix.
##  

## makeCacheMatrix function takes a matrix object(assuming square matrix) as an arguement, stores 
##  its inverse (NULL if inverse is not computed) and  returns a list containing the names of 4 other
##  functions used for purpose of getting or setting either matrix or its inverse.

makeCacheMatrix <- function(x = matrix()) {
 
  mtrxInv<-NULL                                       # Intial value to NULL
  
  
  setMtrx<-function(mx)                                 ## sets x to new matrix mx and reset mtrxInv to NULL
  {
    x<<-mx                                          
    mtrxInv<<-NULL                                 
  }
    
  getMtrx<-function() x                                    ## returns matrix
    
  
  setInv<-function(InvResult) mtrxInv<<-InvResult         ## sets mtrxInv to inverse passed in the arguement
    
  
  getInv<-function() mtrxInv                             ## Returns matrix Inverse
  
  
  ## Return list containing names of the functions 
  
  list(setInv=setInv,getInv=getInv,setMtrx=setMtrx,getMtrx=getMtrx)

}


##  cacheSolve() takes list as an argument, retrieves cached matrix inverse from makeCacheMatrix(). If NULL is returned 
##  this function computes and returns the computed inverse

cacheSolve <- function(x, ...) {


  
    mtrxInv<-x$getInv()                                 ##Retrieve cached matrix inverse if notNULL return cached inverse
    
    if(!is.null(mtrxInv))                           
  {
      message("getting cached result ")
      return(mtrxInv)
    } 
  
   
    mymtrx<-x$getMtrx()   
    
    InvMtrx<-solve(mymtrx)                        ## compute inverse
 
    x$setInv(InvMtrx)                            ## pass to makeCacheMatrix for caching
  
  

        ## Return a matrix that is the inverse of 'x'
        
        return(InvMtrx)
}
