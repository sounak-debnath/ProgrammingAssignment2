## Returns a list of function to evaluate the cached matrix
## setMatrix and getMatrix is used to set and get the cached matrix
## setInverse and getInverse are used to set the get the inverse of the cached matrix
## compareMatrix is used to compare is given matrix is same as the cached matrix
## chk_empty_Inverse is used to check if the cached inverse matrix is empty or not i.e. contains only NAs

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

inv <- matrix()
        getMatrix <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv

#compare if a given matrix arg is equal to the cached matrix
compareMatrix <- function(a){
#check if the args is a matrix
if(!is.matrix(a)){ 
 print("arg supplied is not a matrix")
 return(FALSE)
 }
#initialize counters
cnt <- c(1)
x_list <- as.list(x)
a_list <- as.list(a)
x_row_cnt <- nrow(x)
x_col_cnt <- ncol(x)
a_row_cnt <- nrow(a)
a_col_cnt <- ncol(a)

#check if row and col counts match
if(x_row_cnt != a_row_cnt){
 return(FALSE)
 }
if(x_col_cnt != a_col_cnt){
 return(FALSE)
 }

#Check individual elements now
for(i in x_list){
 if(i!=a_list[cnt]) return(FALSE)
 cnt <- cnt+1
 }
#if we are here, it means matrices are same
return(TRUE)
}

#check for empty matrix
chk_empty_Inverse <- function(){
#check if the args is a matrix
if(!is.matrix(inv)){ 
 print("Inverse is not a matrix")
 return(FALSE)
 }
#initialize counters
inv_list <- as.list(inv)

#Check individual elements now
for(i in inv_list){
 if(!is.na(i)) return(FALSE)
 }
#if we are here, it means matrix is empty i.e. contains only NAs
return(TRUE)
}

setMatrix <- function(y) {
                
                #if the already cached matrix is different from the argument matrix sent, set inverse to empty
                if(!compareMatrix(y)){
                inv <<- matrix()
                }
                x <<- y
        }
        

        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverse = setInverse,
             getInverse = getInverse,
             compareMatrix = compareMatrix,
             chk_empty_Inverse = chk_empty_Inverse)

}


## Function computes inverse of the special matrix
## If inverse is already computed, inverse is retrieved from cache. Else, inverse is computed and set using setInverse
## IMPORTANT NOTE: setMatrix checks if the new matrix being set is different from the one stored in cache - contd..
## If the new matrix being set is different, setMatrix function resets the cached inverse to empty matrix

cacheSolve <- function(x, ...) {
inv <- x$getInverse()
#check if the cached inverse is already computed and matrix has not changed. If so, then use the cached inverse
        if(!x$chk_empty_Inverse()) {
                message("getting cached data")
                return(inv)
        }
        data <- x$getMatrix()
        inv <- solve(data)
        ## set the newly computed inverse matrix
        x$setInverse(inv)
        
        ## Return a matrix that is the inverse of 'x'
        return(inv)
}


