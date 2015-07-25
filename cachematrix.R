## Put comments here that give an overall description of what your
## functions do

#################################################################
## The "makeCacheMatrix" function takes an invertible matrix and
## calculates the inverse which is then cached.
## 
## Arguments:
## matrix_original : An invertible matrix
##
## Returns:
## An object (or whatever R calls it) with internal
## methods (or R's equivalent naming) to set and retrive
## the original and inverted matrix
##
## Author:
## Devon Ly, 2015
#################################################################

makeCacheMatrix <- function(matrix_original = matrix())
{
    ## the example provided for makeVector
    ## really should have line by line commentary of what's going on
    ## was ubber confusing... for the record :)
    
    ## create a variable to store our inverted matrix and
    ## set it to null as default
    matrix_inverse <- NULL
    
    ## define an internal function which can be evoked like a
    ## java method or C++ class function to set the non computed
    ## "original" matrix.
    ## Also calculates the inverse 
    set_matrix <- function(matrix_new)
    {
        matrix_original <<- matrix_new ## Assign our new matrix
        matrix_inverse <<- NULL ## Reset our inversion, in the event that we
                                ## we have a precalculated inversion from a previous run
    }
    
    ## Return whatever we have stored as the original matrix
    get_matrix <- function() matrix_original
    
    ## stores the inverse
    set_inverse <- function(inverse_new) matrix_inverse <<- inverse_new
    
    ## returns the inverted matrix or null if not yet calculated
    get_inverse <- function() matrix_inverse
    
    ## creates a list... to our internal functions
    list(set_matrix = set_matrix,
         get_matrix = get_matrix,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}

#################################################################
## The "cacheSolve" function takes a cached matrix, checking
## if the inverse has already been calculated, it returns the
## precalculated value. Others it performs the inverse calculation.
## 
## Arguments:
## matrix_cached : A cached matrix created from the
## makeCacheMatrix function
##
## Returns:
## The inverted matrix of the original stored within the cached
## matrix
##
## Author:
## Devon Ly, 2015
#################################################################

cacheSolve <- function(matrix_cached, ...)
{
    ## get the stored inverse (if there is one)
    result <- matrix_cached$get_inverse()
    
    ## if the inverse has already been pre-calculated then return then
    ## return the inverted matrix
    if ( !is.null(result) ) 
    {
        message("Getting cached inverted matrix")
        return(result)
    }
    
    ## Calculate a new inverted matrix as one has not been previously calculated
    matrix_original <- matrix_cached$get_matrix() ## Retrieve the original matrix
    result <- solve(matrix_original) ## Calculate the inverse of the matrix
    matrix_cached$set_inverse(result) ## cache the inverted matrix
    result ## Return a matrix that is the inverse of 'x'
}
