##The Project will demonstrates how to get the inverse of a square matrix 
##and store the output in cache and use it if the iverse is again called.
##How to perfom  unit test
##   testMatrix<-makeCacheMatrix(matrix(c(4,3,3,2),nrow=2,ncol=2))
##   testMatrix$get() #get the matrix
##   cacheSolve(testMatrix)  #check cache and return inverse
##   cacheSolve(testMatrix)  #check cache and get the inverse from cache
## Another unit test
##   testMatrix<-makeCacheMatrix(matrix(sample.int(100,size=2000*2000, replace=TRUE),nrow=2000))
##   testMatrix$get() #get the matrix
##   system.time(cacheSolve(testMatrix))  #identify the ellapsed time for calculation
##   system.time(cacheSolve(testMatrix))  #identify the ellapsed time from cache




## makeCacheMatrix take a matrix and cache the matrix inverse as well

makeCacheMatrix <- function(x = matrix()) {
  matrixPlaceHolder<-NULL    #matrixPlaceHolder -- data repository for inverse of matrix
  set<-function(y){          
  x<<-y
 matrixPlaceHolder<<-NULL
}
get<-function() x             #get matrix
setmatrix<-function(solve) matrixPlaceHolder<<- solve   #set the inverse of matrix
getmatrix<-function() matrixPlaceHolder  #return inverse matrix
list(set=set, get=get,
   setmatrix=setmatrix,
   getmatrix=getmatrix)
}

## compute the matrix inverse and check the condition if the matrix inverse exists 
## or not in. if it exist than get it from there.  

cacheSolve <- function(x=matrix(), ...) {
    matrixPlaceHolder<-x$getmatrix() #get the inverse matrix for matrix "x"
    if(!is.null(matrixPlaceHolder)){   #checking condition
      message("getting cached data") 
      return(matrixPlaceHolder)                   
    }
    matrix<-x$get()
    matrixPlaceHolder<-solve(matrix, ...)
    x$setmatrix(matrixPlaceHolder)
    matrixPlaceHolder                  #return value
}

