#' Geomorphometric operations : geodilation
#'
#' Geodilation
#' 
#' @import EBImage 
#' @param marker An image matrix marker
#' @param reference An image matrix reference
#' @param element Structural element
#' @return The geodesic dilation of the image
#' @author Marion Leduc, \email{marion.leduc@gustaveroussy.fr}
#'
#' @export
#'
geodilate = function(marker, reference, element){
  return(pmin(dilate(marker, element), reference))
}

#' Geomorphometric operations : geoerosion
#'
#' Geoerosion
#' 
#' @import EBImage 
#' @param marker An image matrix marker
#' @param reference An image matrix reference
#' @param element Structural element
#' @return The geodesic erosion of the image
#' @author Marion Leduc, \email{marion.leduc@gustaveroussy.fr}
#'
#' @export
#'
geoerode = function(marker, reference, element){
  return(pmax(erode(marker, element), reference))
}

#' Geomorphometric operations : reconstruction
#'
#' Image reconstruction after geomorphometric operation
#' 
#' @import EBImage 
#' @param f the function to be applied 
#' @param marker An image matrix marker
#' @param reference An image matrix reference
#' @param element Structural element
#' @param oc Can be NULL, 'open' or 'close'; default is NULL
#' @return The geodesic erosion of the image
#' @author Marion Leduc, \email{marion.leduc@gustaveroussy.fr}
#'
#' @details
#'if oc is set to \code{\link{open}}, then \code{\link{geodilate}} shall be used as the working function;
#'if oc is set to \code{\link{close}}, then \code{\link{geoerode}} shall be used as the working function;
#'if oc is set to NULL, a marker image shall be indicated in the option 'marker'
#' @export
#'
MorphRecons = function(f,reference,element,marker=NULL,oc=NULL){    
  
  if(!is.null(oc)){
    if(oc=='open'){
      marker=erode(reference,element)
    }else{
      marker=dilate(reference,element)
    }
    recons = marker
  }else{
    if(!is.null(marker)){
      recons = marker
    }else{
      print("Please indicate a marker image, or set oc to 'open' or 'close'")
    }
  }

    result = reference
  
  while(any(recons != result)){
    result = recons
    recons = f(result, reference, element)
  }
  return(result)
}










