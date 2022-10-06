#' Resize image by padding or cropping.
#'
#' 
#' @import EBImage 
#' @param x An input (image) matrix
#' @param target_size The target size of the resulting image matrix
#' @param value	The pixel value to be added. If absent, the median from the original image will be used
#' @return A centered resized matrix 
#' @author Allan Sauvat, \email{allan.sauvat@gustaveroussy.fr}
#'
#' @export
#'

resize_pad = function(x,target_size,value){
  if(missing(value))value=median(x)
  if(nrow(x)==target_size[1] & ncol(x)==target_size[2]){
    y = x
  }else{
    # dimension x
    if(target_size[1]>=nrow(x)){
      padx = (target_size[1]-nrow(x))%/%2
      padx = c(padx,target_size[1]-nrow(x)-padx)
      y = do.call('rbind',list(matrix(value,nrow=padx[1],ncol=ncol(x)),x,matrix(value,nrow=padx[2],ncol=ncol(x))))
    }else{
      xc = nrow(x)/2
      padx = (target_size[1])%/%2
      padx = c(padx,target_size[1]-padx)
      y = x[(xc-padx[1]):(xc+padx[2]-1),]
    }
    # dimension y
    if(target_size[2]>=ncol(x)){
      pady = (target_size[2]-ncol(y))%/%2
      pady = c(pady,target_size[2]-ncol(y)-pady)
      y = do.call('cbind',list(matrix(value,ncol=pady[1],nrow=nrow(y)),y,matrix(value,ncol=pady[2],nrow=nrow(y))))
    }else{
      yc = ncol(y)/2
      pady = (target_size[2])%/%2
      pady = c(pady,target_size[2]-pady)
      y = y[,(yc-pady[1]):(yc+pady[2]-1)]
    }
  }
  # return
  return(y)
}
