#Create a matrix then inverse it using solve function. If the inverse is in the
#cache(or the matrix has not been changed) no new calculation is performed. instead
#the cached data will be return

makeCacheMatrix <- function(x = matrix()) {
        inverse_mat <- NULL # create undefined/empty "slot" for the answer
        
        # set a new matrix to global x, and reset(delete) the inverse mat
        set_mat <- function(y){ 
                x <<- y
                inverse_mat <<- NULL
        }
        
        # return the matrix, will be assigned to other var in other function
        get_mat <- function()x
        
        
        # to save the answer to the empty "slot" that is created previously
        set_inv <- function(ans)inverse_mat<<-ans
        
        #return the inverse matrix 
        get_inv <- function()inverse_mat
        
        # make a list so that can be called/invoked(ex:x$get_mat)
        list(
                set_mat = set_mat ,
                get_mat = get_mat ,
                set_inv = set_inv ,
                get_inv = get_inv
        )
        
}


#this function will return the inverse matrix. First, it will check weather any
#values is already store in the "slot" inverse_mat and will return the answer ,if
#the slot already has the answer. ELSE it will solve the new matrix and then store 
# it in the "slot" 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inverse_mat <- x$get_inv()
        
        if(!is.null(inverse_mat)){ #check if it is undefined
                message("getting data from cache")
                return(inverse_mat) #return the answer and exit
                
        }
        
        #if not exit early all these codes below will be run
        data <- x$get_mat() 
        inverse_mat <- solve(data, ...) # solve 
        x$set_inv(inverse_mat) # store the answer
        inverse_mat 
        
}
