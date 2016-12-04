## The two functions defined below work together to compute and save 
## cached values of the inverse of a matrix. The purpose is to improve 
## the efficiency of a script by using cached values instead of 
## recalculating them. 


## The function makeCacheMatrix creates a special List containing 4 functions 
## designed to set and retrieve (set, get) the values of a matrix and its
## inverse (setinv, getinv)

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }	
	get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)
}

## The cacheSolve function takes as input the List created by the 
## makeCacheMatrix function. It first checks for the existence of 
## a cached value of the inverse, and returns that value if it exists. 
## If not, the solve() function is used to compute the inverse, the value  
## is cached using setinv, and the inverse is returned. 
## Note: it is assumed that the matrix x is always invertible. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinv(i)
        i		
}
