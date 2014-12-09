## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special matrix object that caches an inverse of matrix
## the object 4 methods get, set, getinverse, setinverse

makeCacheMatrix <- function(x = matrix()) {
        # intitalize inv to NULL
	inv <- NULL
        set <- function(y){
            x <<- y
            inv <<- NULL
	}
        get <- function() x  # return the matrix
	setinverse <- function(inverse) inv <<- inverse  # assign inverse to inv
	getinverse <- function() inv  # return the inverse
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## cacheSolve returns a matrix which is inverse of x
## The function does a check if the inverse already exists
## in such a case it will print out "getting cached inverse of matrix"
## and return inverse of matrix
## if the inverse is not cached the function uses solve to compute the inverse
## and then assign it to x using setinverse property

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)){
		message("getting cached inverse of matrix")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data) #do an inverse
        x$setinverse(inv)
        inv
}

# tests on R command line that the code works as expected
c = rbind(c(1, -1/4), c(-1/4, 1))
test <- makeCacheMatrix()

test$set(c)
test$get()

# output
#       [,1]  [,2]
# [1,]  1.00 -0.25
# [2,] -0.25  1.00

inverse <- cacheSolve(test)

# output
#          [,1]      [,2]
# [1,] 1.0666667 0.2666667
# [2,] 0.2666667 1.0666667

# test caching 
inverse <- cacheSolve(test)

# output
# getting cached inverse of matrix

inverse
# output
#          [,1]      [,2]
# [1,] 1.0666667 0.2666667
# [2,] 0.2666667 1.0666667
