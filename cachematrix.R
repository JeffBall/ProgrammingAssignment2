
##  the function makeCacheMatrix is going to take a matrix
##  as its argument and create four functions for it:

##  set = set the value of the matrix in another
##  environment (using the <<- operator)

##  get = simply return the matrix

##  setinv = set the value of the object InvMat

##  getinv = retrieve the value of the object InvMat

##  note that no calculations happen in this function; its
##  purpose is to manage variables

makeCacheMatrix <- function(x = matrix()) {

#   first, let's initialize the object that will store the
#   answer for the inverse matrix
    
    InvMat <- NULL
    
#	now we're going to create the 'set' function, which will
#	basically set the value of x (in another environment)
#	to the value passed in the 'set' function and it will also
#	set the variable InvMat (which is in a parent environment in
#	this case) to NULL
    
    set <- function(y)
    {
        x <<- y
        InvMat <<- NULL
    }
    
#	get() doesn't take any arguments and just returns the value
#	of x
    
    get <- function() x
    
#	setinv(inv) is going to set the value of InvMat (in another
#	environment) equal to the value passed into setinv
#	remember that it's the variable on the left that is in 
#	another environment, not the one on the right
    
    setinv <- function(inv) InvMat <<- inv
    
#	getinv() is not going to pass any arguments and basically
#	returns the value of InvMat
    
    getinv <- function() InvMat
    
#	so, InvMat is the variable that is going to hold the value of the
#	inverse, and wherever "InvMat <<- " is used, it means that it's in
#	a different environment

#   finally, create a list that contains the various functions...    

    list(set=set,get=get,setinv=setinv,getinv=getinv)
}

##  cacheSolve is going to find the inverse of the matrix x
##  but first, it will check to see if the answer, stored
##  in the object InvMat, has a value already.  if so, use
##  that value.  if not, solve it and store it for future
##  use

cacheSolve <- function(x, ...) {
#	this is where the actual calculations happen.
#	the makeCacheMatrix function isn't doing any
#	calculations; it's pretty much managing the
#	variables.  so, let's start by setting the
#	value of InvMat - does it exist or not?
    
    InvMat <- x$getinv()
    
#	this checks to see if the inverse has been calculated
#	yet or not.  if it isn't null, then it's going to
#	provide the value of the inverse and then exit out
    
    if(!is.null(InvMat))
    {
        message("Getting cached data for the matrix inverse")
        return(InvMat)
    }
    
#	now let's see where it's getting the data values; it's
#	calling the get function, so let's review what get()
#	is supposed to return
    
    data <- x$get()
    
#	finally, the moment we've been waiting for.  Calculate the
#	matrix inverse already!

    InvMat <- solve(data,...)
    
#	we took the time to calculate the inverse since it hadn't
#	been calculated yet; let's make sure to store it
    
    x$setinv(InvMat)

#	the last statement evaluated will be what's returned, so
#	we'll show the value of InvMat
    
    InvMat
}
