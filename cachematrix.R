## These two functions create, store and retrieve a matrix and its inverse in/from cache  

## makeCacheMatrix function creates a custom matrix object that can cache its inverse
## It has four member functions : set, get, setInverse, getInverse
## set stores the matrix in cache, get retrieves the matrix
## setInverse and getInverse do the same but for the inverse of the input matrix
      
makeCacheMatrix <- function(x = matrix()){    
       m <- NULL            # result of inversion is stored here
       
       # setter function sets a matrix to object created by makeCacheMatrix function
       set <- function(y){  
           x <<- y  
           m <<- NULL 
       }
       
       get <- function() x                       # returns the input matrix
       setInverse <- function(solve) m<<- solve  # sets the inversed matrix
       getInverse <- function() m                # returns the inversed matrix
       list(set = set, get = get,
                    setInverse = setInverse,
                    getInverse = getInverse)     # creates a list of functions
      }
    
 
## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix function
## it checks if the inverse has been calculated before
## if calculated before, it retrieves the data from the cache 
## if not calculated, inverse of matrix is calculated and stored it in the cache
      
cacheSolve <- function(x, ...) {
       ## Return a matrix that is the inverse of 'x'
       m <- x$getInverse()                   # query the cache of matrix x
       if(!is.null(m)){                      # if there is a cache the inverse has been already calculated
           message("getting cached data")    # send message that it is a cache 
           return(m)                           
         }
       data <- x$get()                     # if not, get the matrix object
       m <- solve(data, ...)               # calculate the inverse
       x$setInverse(m)                     # store the inverse matrix in cache using setter function
       m                                   
      }