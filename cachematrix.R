## The functions makeCacheMatrix and cacheSolve are used to 
## calculate the inverse of a given matrix and then return its 
## inverse as mat_inverse.  
## makeCacheMatrix returns a list containing the four functions:
## set, getMat, setInverse and getInverse; which are available for
## use by cacheSolve. cacheSolve uses the functions created above, 
## along with the solve() function, to either retrieve the cached value 
## for mat_inverse and return it with a message, or calculate the inverse
## for the matrix, return it, and cache its value for future use.



## makeCacheMatrix creates/constructs the following 4 functions:
## set, getMat, setInverse and getInverse and then returns them
## as a list of fucntions which can then be called by other 
## programs/functions.

makeCacheMatrix <- function(x = matrix()) {
		mat_inverse <- NULL
        # initializes the local variable mat_inverse to Null (within the
        # the makeCacheMatrix function); this variable can be reset
        # in the local environment of makeCacheMatrix by the set function. 
        
        set <- function(y) {
                # set function allows you to reset the values of x and 
                # mat_inverse without having to re-run makeCacheMatix
                x <<- y
                mat_inverse <<- NULL
        }
        
        getMat <- function() x
        # returns the value of x when the getMat function is called; since
        # it isn't defined within the function, it will look to the enclosing
        # function environments (in this case makeCacheMatrix)
        
        setInverse <- function(solve) mat_inverse <<- solve
        # sets the current value of mat_inverse in the containing environment
        # (makeCacheMatrix) to the value of the formal argument, solve.
        # This can be used to cache the value of mat_inverse for later use.
        
        getInverse <- function() mat_inverse
        # returns the current value of mat_inverse; since it is not defined in
        # the function, it will look to the enclosing function (makeCacheMatrix)
        # to find the value of mat_inverse
        
        # a list containing the four functions created by this function is
        # returned
        list(set = set, 
             getMat = getMat,
             setInverse = setInverse,
             getInverse = getInverse)
}

## cacheSolve checks the value of mat_inverse in the 
## environment makeCacheMatrix.  If it is NULL, it uses the solve() 
## function to calculate the inverse of the matrix, and assigns this
## to the variable mat_inverse, which is then returned.
        
## If mat_inverse has already been calculated (is not equal to NULL),
## then it returns the cached value of mat_inverse along wth a message
## indicating this.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mat_inverse <- x$getInverse()
        ## get_inverse() returns the current value of mat_inverse  
        ## found in the enclosing environment of getInverse (makeCacheMatrix)
        
        if(!is.null(mat_inverse)) {
                # if mat_inverse has already been calculated, then
                # the cached value of mat_inverse is returned along
                # with a message indicating this. The function is exited
                # at this point.  If mat_inverse is NULL, then this section
                # is bypassed.
 
                message("getting cached inverse matrix")
                return(mat_inverse)
        }
        

        data <- x$getMat()
        ## if mat_matrix is NULL, then data is set to the value of the 
        ## matrix which is the formal argument of the function
        ## makeCacheMatrix.
        
        mat_inverse <- solve(data, ...)
        ## if mat_matrix is NULL, the function solve() is called to calculate
        ## the inverse of the matrix (data) and assign this value to the variable
        # mat_inverse
        
        x$setInverse(mat_inverse)
        ## the value of the newly determined inverse matrix mat_inverse is 
        ## cached in the enclosing environment (makeCacheMatrix)
        
        mat_inverse
        ## the newly calculated inverse matrix is returned by this function
}