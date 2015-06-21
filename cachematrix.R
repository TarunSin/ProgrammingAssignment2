## Overall the functions meet the requirements of the assignment. They help in 
## finding the inverse of the matrix and also caches the result in case the same
## is required. Please note the coding has been limited to meet the
## assignment requirements without going into elaborate validations

## Pass the given matrix to first function makeCacheMatrix. The resultant would be 
## list of functions. The list is passed to second function cacheSolve to generate
## the inverse of matrix. It also checks if the result is already cached or not


## makeCacheMatrix - is the function which creates special matrix object for given
## marrix. The special object is the list of functions which help in setting & 
## getting the matrix and also caching the object while setting inverse. It creates
## an environment where the list objects & variables defined in it reside.

## say mat1 <- matrix(1:4, nrow = 2, ncol = 2)
## mat2 <- makeCacheMatrix(mat1). mat2 will be list of 4 functions having same env
## Here they are > mat2
# $set
# function (y) 
# {
#     x <<- y
#     s <<- NULL
# }
# <environment: 0x0000000008264c50>
#     
#     $get
# function () 
#     x
# <environment: 0x0000000008264c50>
#     
#     $setinverse
# function (solve) 
#     s <<- solve(x)
# <environment: 0x0000000008264c50>
#     
#     $getinverse
# function () 
#     s
# <environment: 0x0000000008264c50>


makeCacheMatrix <- function(x = matrix()) {
    
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
        
    }
    
    get <- function() x
    setinverse <- function(solve) s <<- solve(x)
    getinverse <- function() s
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## cacheSolve takes the list output of first function as input. It checks if the
## if the result is already available or not. First time it will call the 
## setinverse function of makeCacheMatrix  to find inverse. If the call is made again
## it will show the message getting cached data.

# mat3 <- cacheSolve(mat2)
# mat3 = 
#       [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# mat3 <- cacheSolve(mat2)
# Result is "getting cached data" as expected


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    s <- x$getinverse()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    
    data <- x$get()
    s <- solve(data,...)
    x$setinverse(s)
    s
}
