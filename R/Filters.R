#' Digital filtering : Laplacian filter
#'
#' Laplacian filter
#' 
#' @import EBImage 
#' @param x An image matrix
#' @param direction direction for Sobel edges detection. Can be 'left', 'right', or 'both'
#' @param choice What discrete kernel shall be used if several available#' 
#' @return The convolved image 
#' @author Allan Sauvat, \email{allan.sauvat@gustaveroussy.fr}
#'
#' @export
#'
Lap=function(x,choice=1,...){
  if(choice==1){
    fi = matrix(c(-1,-1,-1,-1,8,-1,-1,-1,-1),nrow=3)
  }else{
    fi=matrix(c(0,-1,0,-1,4,-1,0,-1,0),nrow=3)
  }
  return(filter2(x,fi,...))
}


#' Digital filtering : Laplacian of Gaussian
#'
#' LoG filter
#' 
#' @import EBImage 
#' @param x An image matrix
#' @param direction direction for Sobel edges detection. Can be 'left', 'right', or 'both'
#' @param choice What discrete kernel shall be used if several available#' 
#' @return The convolved image 
#' @author Allan Sauvat, \email{allan.sauvat@gustaveroussy.fr}
#'
#' @export
#'
LoG=function(x,...){
  y=Lap(gblur(x,...))
  y[which(y<0)]=0
  return(y)
}


#' Digital filtering : Sharpen
#'
#' Sharpen
#' 
#' @import EBImage 
#' @param x An image matrix
#' @param direction direction for Sobel edges detection. Can be 'left', 'right', or 'both'
#' @param choice What discrete kernel shall be used if several available#' 
#' @return The convolved image 
#' @author Allan Sauvat, \email{allan.sauvat@gustaveroussy.fr}
#'
#' @export
#'
Sharp=function(x,...){
  y=filter2(x,1/16*matrix(c(-1,-2,-1,-2,12,-2,-1,-2,-1),nrow=3))
  return(y)
}


#' Digital filtering : Edges detection
#'
#' Sobel filter
#' 
#' @import EBImage 
#' @param x An image matrix
#' @param direction direction for Sobel edges detection. Can be 'left', 'right', or 'both'
#' @return The convolved image 
#' @author Allan Sauvat, \email{allan.sauvat@gustaveroussy.fr}
#'
#' @export
#'
Sobel=function(x,direction='both',...){
  
  lfi = matrix(c(1,0,-1,2,0,-2,1,0,-1),nrow=3)
  if(direction=='both'){
    y=filter2(filter2(x,lfi),-lfi)
  }else{
    if(direction=='left'){
      y=filter2(x,lfi)
    }else{
      y=filter2(x,-lfi)
    }
  }
  return(y)
  
} 


#' Digital filtering : Low Pass filter
#'
#' Low Pass filter
#' 
#' @import EBImage 
#' @param x An image matrix
#' @return The convolved image 
#' @author Allan Sauvat, \email{allan.sauvat@gustaveroussy.fr}
#'
#' @export
#'
LowPass=function(x,...){
  fi=1/9*(matrix(rep(1,9),nrow=3))
  y=filter2(x,fi)
  y[which(y<0)]=0
  return(y)
}


#' Digital filtering : High Pass filter
#'
#' High Pass filter
#' 
#' @import EBImage 
#' @param x An image matrix
#' @return The convolved image 
#' @author Allan Sauvat, \email{allan.sauvat@gustaveroussy.fr}
#'
#' @export
#'
HighPass=function(x,...){
  fi=1/9*(matrix(c(-1,-1,-1,-1,8,-1,-1,-1,-1),nrow=3))
  y=filter2(x,fi)
  y[which(y<0)]=0
  return(y)
}
