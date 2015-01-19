## makeCacheMatrix function
# creates a special "matrix", which is really a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the solve (inverse of matrix)
# 4. get the value of the solve 
 
makeCacheMatrix <- function(x = matrix()) { 
  inv <- NULL 
  set <- function(y) { 
    x <<- y 
    inv <<- NULL 
   } 
   get <- function() x 
   setsolve <- function(solve) inv <<- solve 
   getsolve <- function() inv 
   list(set = set, get = get, 
        setsolve = setsolve, 
        getsolve = getsolve) 
 } 
 

# cacheSolve function
# calculates the inverce matrix of the special "matrix" with 
# the above function.

cacheSolve <- function(x, ...) { 
   inv <- x$getsolve() 
   if(!is.null(inv)) { 
     message("getting cached data") 
     return(inv) 
   } 
   data <- x$get() 
   inv <- solve(data, ...) 
   x$setsolve(inv) 
   inv 
 } 

# main prog
x=matrix(1:4,2,2,byrow=T)
res=makeCacheMatrix(x)
res$get()
solve(x)
cacheSolve(res)
cacheSolve(res)






