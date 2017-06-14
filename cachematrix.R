## Caching the Inverse of a Matrix
## Written by Amar Faiz Zainal Abidin (amarfaiz[at]utem.edu.my)
## Written on 14 June 2017

## The main objective of this assignment is to utilize the scoping rules of the R languages for decreasing the computation time of a computed inverse matrix by retriving the available result
## Similar to example given earlier makeVector, the makeCacheMatrix creates a special "vector" which is a list containing a function to:
## 1. Set the value of the vector by using variable set
## 2. Get the value of the vector by using variable get
## 3. Set the value of the inverse matrix by using variable setInvMat
## 4. Get the value of the inverse matrix by using variable getInvMat

makeCacheMatrix <- function(x = matrix()) 
{
	u <- NULL
	set <- function(y)
	{
		x <<- y
		u <<- NULL
	}
	get <- function() 
	{
		x
	}
	setInvMat <- function(inverse)
	{
		u <<- inverse
	}
	getInvMat <- function()
	{
		u
	}
	list(set = set, get = get, setInvMat = setInvMat, getInvMat = getInvMat)
}


## cacheSolve function check whether the inverse matrix has been computed, If so, it gets the inverse matrix from the cache. Otherwise, it will compute

cacheSolve <- function(x, ...) 
{
	u <- x$getInvMat()
	if(!is.null(u))
	{
		message("Getting cached data!")
		return(u)
	}
	data <- x$get()
	u <- solve(data, ...)
	x$setInvMat(u)
	u
}
