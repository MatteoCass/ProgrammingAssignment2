## Given a square invertible matrix x, we create a "special" matrix object that can:
## 1) retrieve the square invertible matrix
## 2) change the square invertible matrix
## 3) retrieve the value of its inverse
## 4) store the value of its inverse
## and a function that computes the inverse of the matrix unless the inverse is already stored 
## in the cache of the "special" matrix object (or the square invertible matrix has changed).


## Creates a list of 4 elements to retrieve / change the values of a square invertible matrix, 
## and store / retrieve its inverse in the cache (Inv). Input argument is a square invertible matrix x.

makeCacheMatrix <- function(x = matrix()) {
        ## initialize object Inv to store the inverse
        Inv <- NULL
        ## change the x to newX with x$setMat(newX) and reset the inverse to NULL
        setMat <- function(newX = matrix()){
                x <<- newX
                Inv <<- NULL
        }
        ## retrieve the square invertible matrix x (to compute inverse)
        getMat <- function(){
                x
        }
        ## store the inverse into object Inv
        setInv <- function(inverse){
                Inv <<- inverse
                
        }
        ## retrieve the inverse (Inv)
        getInv <- function(){
                Inv
        }
        ## create the list
        list(setMat = setMat, getMat = getMat, setInv = setInv, getInv = getInv) 
        
}


## Seeks in the cache of the "special" matrix object created with makeCacheMatrix for a stored inverse
## matrix (Inv). If Inv is NULL, computes the inverse and stores it in Inv. Outputs the inverse (Inv).
## NB The argument x in input is the "special" matrix object created with the function makeCacheMatrix,
## it is not the matrix x given as input argument to makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## retrieves the object Inv from the cache
        Inv <- x$getInv()
        ## check if Inv contains something. If yes, retrieve it, send a message and exit the function
        if(!is.null(Inv)){
                print("Getting cached inverse")
                return(Inv)
        }
        ## if Inv was NULL, retrieve the data, calculate the inverse and store it in Inv and the cache
        data <- x$getMat()
        Inv <-solve(data)
        x$setInv(Inv)
        
        ## Return a matrix that is the inverse of 'x'
        Inv
}