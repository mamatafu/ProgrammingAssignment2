## Creates a special "matrix" object that can cache its inverse. 
3 
 
4 makeCacheMatrix <- function(x = matrix()) { 
5         i  <- NULL 
6         set  <- function(y){ 
7                 x <<- y 
8                 i <<- NULL  
9         } 
10         get  <- function() x 
11         setinverse  <- function(inverse) i  <<- inverse 
12         getinverse  <- function() i 
13         list(set= set, get = get,  
14              setinverse = setinverse,  
15              getinverse = getinverse) 
16 
 
17 } 
18 
 
19 
 
20 ## Computes the inverse. If the inverse has already been calculated (and the matrix has not changed), 
21 ## then the cachesolve should retrieve the inverse from the cach 
22 
 
23 cacheSolve <- function(x, ...) { 
24         i  <- x$getinverse() 
25         if (!is.null(i)){ 
26                 message("getting cached data") 
27                 return(i) 
28         } 
29         data  <- x$get() 
30         i  <- solve(data, ...) 
31         x$setinverse(i) 
32         i 
33 } 
