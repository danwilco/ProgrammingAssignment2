## This function uses the example provided by makeVector and cachemean to create a function that takes an input of 
## a matrix and calculates the inverse of the matrix, however first it checks to see, using the makeCacheMatrix function
## whether the inverse of the matrix has been calculated and stored before.

## comments related to this function are provided throughout

makeCacheMatrix <- function(x = matrix()) { # define x as an empty matrix
        m <- NULL # creates empty variable inside the makeCacheMatrix environment
        set <- function(y) { # defines set function to set (mutate) variables within makeCacheMatrix environment
                x <<- y ## assigns y to x in the parent environment
                m <<- NULL ## sets m to null clearing any other stored version
        }
        get <- function() x # function which retrieves value of x from parent environment
        setsolve <- function(solve) m <<- solve # this defines the setter for solve, assigns the value of solve to m
        getsolve <- function() m # retrieves m from parent environment and assigns to getsolve
        list(set = set, get = get, # produces list of all variables to access outside function
             setsolve = setsolve,
             getsolve = getsolve)
}
cacheSolve <- function(x, ...) {
        m <- x$getsolve() # retrieves value of solve from parent environment
        if(!is.null(m)) { # checks if content of m is null or not
                message("getting cached matrix") # if not null, then returns cached matrix from m
                return(m)
        } # if null function continues
        data <- x$get() # assigns x from parent environment using getter function to data
        m <- solve(data, ...) # solves the inverse of data and assigns result to m
        x$setsolve(m) # uses setter function to assign value of m to list in parent environment
        m # returns m
}