 my_Deparse <- function( Code ){
   # Remove the first and last lines of the inpout, which are { and }, grouping the code
   len <- length( deparse(Code)) 
   new_Code <- deparse(Code)[2 : (len - 1) ]
   new_Code
 }
 