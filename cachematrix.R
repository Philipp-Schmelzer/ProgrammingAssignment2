## Put comments here that give an overall description of what your
## functions do

# in this code there are two functions: makeCacheMatrix and cacheSolve.
#the first one consists out of 4 elements: set, get, setInverse and getInverse.
# Assumption: Matrix is invertible

makeCacheMatrix <- function(x = matrix()){
  inv <-NULL #initalizing inverse as NULL
  set <- function(y) {
    x<<-y
    inv <<- NULL
  } #set the value of the matrix
  
  get <- function() {x} #function to get value of matrix
  setInverse <- function(inverse) {inv <<- inverse} #function to set value of inverse
  getInverse <- function() {inv} #function to get value of inverse
  list(set=set, get=get, setInverse = setInverse, getInverse = getInverse) #create list
}

cacheSolve <- function(x, ...) #function fpr getting cache data
{
  inv <- x$getInverse()
  if(!is.null(inv)) #checking if inverse matrix is null
  {
    message("Getting cached data")
    return(inv) #gives back inverse values
  }
  matrice <- x$get()
  inv <- solve(matrice, ...) #calculate the inverse values of original values
  x$setInverse(inv)
  inv #return the inverse matrix of original matrix
}