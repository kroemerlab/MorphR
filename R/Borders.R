#' Add borders to matrix
#'
#' 
#' @import EBImage 
#' @param mat An input (image) matrix
#' @param int The intger to fill the matrix with
#' @param size Border thickness 
#' @param outside Shall borders be drawn outside or inside the matrix ?
#' @return A mtrix with added borders 
#' @author Allan Sauvat, \email{allan.sauvat@gustaveroussy.fr}
#'
#' @export
#'

addbord=function(mat,int=1,size=1,outside=T){
  
  if(outside){
  vb=matrix(int,nrow=nrow(mat),ncol=size)
  hb=matrix(int,nrow=size,ncol=ncol(mat)+2*size)
  return(rbind(hb,cbind(vb,mat,vb),hb))
  }else{
    mat[c(1:size,(nrow(mat)-size):nrow(mat)),]=int
    mat[,c(1:size,(ncol(mat)-size):ncol(mat))]=int
    return(mat)
  }
}

